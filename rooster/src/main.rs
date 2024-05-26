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
        notify_completion_send.send(()).await;
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
                    labels.push(ariadne::Label::new(("test.roo", label.0.clone()))
                        .with_message(label.1.clone())
                        .with_color(colors[n])
                    );
                }
                // TODO: get filename somehow
                let mut r = ariadne::Report::<(&str, Range<usize>)>::build(ariadne::ReportKind::Error, "test.roo", report.offset)
                    .with_message(report.message)
                    .with_labels(labels);
                if let Some(note) = report.note {
                    r.set_note(note);
                }
                if let Some(help) = report.help {
                    r.set_help(help);
                }
                r
                    .finish()
                    .eprint(("test.roo", ariadne::Source::from(include_str!("../../test.roo"))))
                    .unwrap();
            },
        };
    }
    task_handle.await;
}
