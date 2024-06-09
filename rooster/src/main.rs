#[cfg(feature = "annotate-snippets")]
use annotate_snippets::Level;
#[cfg(feature = "annotate-snippets")]
use annotate_snippets::Renderer;
#[cfg(feature = "annotate-snippets")]
use annotate_snippets::Snippet;
#[cfg(feature = "ariadne")]
use ariadne::Fmt;
use std::ops::Range;
use tokio::sync::mpsc;

#[cfg(all(feature = "ariadne", feature = "annotate-snippets"))]
compile_error!("please select only one compiler diagnostics backend in crate features");

fn print_usage() {
    println!("Usage: rooster <filename>")
}

fn error_incorrect_args() {
    println!("Error: incorrect arguments to program.");
    print_usage();
    panic!();
}

#[cfg(feature = "ariadne")]
fn format_text(text: String) -> String {
    let mut tokens = text.split('`').map(|x| x.to_string()).collect::<Vec<_>>();
    for n in 0..tokens.len() {
        if n % 2 == 1 {
            tokens[n] = format!(
                "{}",
                tokens[n].clone().fg(ariadne::Color::Rgb(64, 196, 196))
            );
        }
    }
    tokens.join("")
}

#[cfg(feature = "ariadne")]
async fn print_report(report: rooster_parser::Report) {
    let mut color_generator = ariadne::ColorGenerator::new();
    let colors = vec![
        color_generator.next(),
        color_generator.next(),
        color_generator.next(),
        color_generator.next(),
    ];
    let mut labels = vec![];
    for n in 0..report.labels.len() {
        let label = &report.labels[n];
        labels.push(
            ariadne::Label::new((&*report.filename, label.0.clone()))
                .with_message(format_text(label.1.clone()))
                .with_color(colors[n]),
        );
    }
    // TODO: get filename somehow
    let kind = if report.is_error {
        ariadne::ReportKind::Error
    } else {
        ariadne::ReportKind::Warning
    };
    let mut r =
        ariadne::Report::<(&str, Range<usize>)>::build(kind, &*report.filename, report.offset)
            .with_message(format_text(report.message))
            .with_labels(labels);
    if let Some(note) = report.note {
        r.set_note(format_text(note));
    }
    if let Some(help) = report.help {
        r.set_help(format_text(help));
    }
    r.finish()
        .eprint((
            &*report.filename,
            ariadne::Source::<String>::from(
                (&*rooster_parser::get_contents(&report.filename)
                    .await
                    .unwrap())
                    .to_string(),
            ),
        ))
        .unwrap();
}

#[cfg(feature = "annotate-snippets")]
async fn get_snippet(range: Range<usize>, filename: &str) -> (String, usize, usize) {
    let source = rooster_parser::get_contents(filename).await.unwrap();
    let mut line_number = 1;
    let mut last_line_start = 0;
    let mut offset = 0;
    let mut first_line = 1;
    let mut end_found = false;
    let mut append = false;
    let mut r = vec![];
    let mut n = 0;
    for ch in source.chars() {
        if n == range.start {
            offset = last_line_start;
            first_line = line_number;
            append = true;
        }
        if n == range.end {
            end_found = true;
        }
        r.push(ch);
        if ch == '\n' {
            if end_found {
                break;
            }
            if !append {
                r.clear();
            }
            line_number += 1;
            last_line_start = n + 1;
        }
        n += 1;
    }
    let snippet = r.into_iter().collect();
    (snippet, first_line, offset)
}

#[cfg(feature = "annotate-snippets")]
async fn print_report(report: rooster_parser::Report) {
    let mut start = report.offset;
    let mut end = report.offset;
    for label in &report.labels {
        start = start.min(label.0.start);
        end = end.max(label.0.end);
    }
    let (snippet, first_line, snippet_offset) = get_snippet(start..end, &report.filename).await;
    let mut annotations = vec![];
    let level = if report.is_error {
        Level::Error
    } else {
        Level::Warning
    };
    for n in 0..report.labels.len() {
        let range =
            report.labels[n].0.start - snippet_offset..report.labels[n].0.end - snippet_offset;
        let annotation = level.span(range).label(&report.labels[n].1);
        annotations.push(annotation);
    }
    let renderer = Renderer::styled();
    let message = level.title(&report.message).snippet(
        Snippet::source(&snippet)
            .line_start(first_line)
            .origin(&report.filename)
            .fold(true)
            .annotations(annotations),
    );
    eprintln!("{}", renderer.render(message));
    if let Some(note) = report.note {
        let mut lines = note.split("\n").collect::<Vec<_>>();
        let title = lines.remove(0);
        let content = lines.join("\n").split('`').collect::<Vec<_>>().join("");
        let message = Level::Note.title(&title).snippet(Snippet::source(&content));
        eprintln!("{}", renderer.render(message));
    }
    if let Some(help) = report.help {
        let mut lines = help.split("\n").collect::<Vec<_>>();
        let title = lines.remove(0);
        let content = lines.join("\n").split('`').collect::<Vec<_>>().join("");
        let message = Level::Help.title(&title).snippet(Snippet::source(&content));
        eprintln!("{}", renderer.render(message));
    }
}

#[tokio::main]
async fn main() {
    let mut args = std::env::args();
    if args.len() != 2 {
        error_incorrect_args();
    }
    let logical_path = args.nth(1).unwrap();
    // we use a mpsc because Notify is not cancellation-safe
    let (notify_completion_send, mut notify_completion) = mpsc::channel(1);
    let task_handle = tokio::spawn(async move {
        let _ = rooster_parser::verify(&logical_path).await;
        notify_completion_send.send(()).await.unwrap();
    });
    let mut report_receiver = rooster_parser::REPORTS.subscribe();
    loop {
        tokio::select! {
            _ = notify_completion.recv() => break,
            report = report_receiver.recv() => {
                let report = report.unwrap();
                print_report(report).await;
            },
        };
    }
    task_handle.await.unwrap();
}
