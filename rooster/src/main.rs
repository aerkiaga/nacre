use ariadne::Fmt;
use std::ops::Range;
use tokio::sync::mpsc;

fn print_usage() {
    println!("Usage: rooster <filename>")
}

fn error_incorrect_args() {
    println!("Error: incorrect arguments to program.");
    print_usage();
    panic!();
}

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
        rooster_parser::get_expression(&logical_path).await;
        notify_completion_send.send(()).await.unwrap();
    });
    let mut report_receiver = rooster_parser::REPORTS.subscribe();
    let mut color_generator = ariadne::ColorGenerator::new();
    let colors = vec![
        color_generator.next(),
        color_generator.next(),
        color_generator.next(),
        color_generator.next(),
    ];
    loop {
        tokio::select! {
            _ = notify_completion.recv() => break,
            report = report_receiver.recv() => {
                let report = report.unwrap();
                let mut labels = vec![];
                for n in 0..report.labels.len() {
                    let label = &report.labels[n];
                    labels.push(ariadne::Label::new((&*report.filename, label.0.clone()))
                        .with_message(format_text(label.1.clone()))
                        .with_color(colors[n])
                    );
                }
                // TODO: get filename somehow
                let mut r = ariadne::Report::<(&str, Range<usize>)>::build(ariadne::ReportKind::Error, &*report.filename, report.offset)
                    .with_message(format_text(report.message))
                    .with_labels(labels);
                if let Some(note) = report.note {
                    r.set_note(format_text(note));
                }
                if let Some(help) = report.help {
                    r.set_help(format_text(help));
                }
                r
                    .finish()
                    .eprint((&*report.filename, ariadne::Source::<String>::from((&*rooster_parser::get_contents(&report.filename).await.unwrap()).to_string())))
                    .unwrap();
            },
        };
    }
    task_handle.await.unwrap();
}
