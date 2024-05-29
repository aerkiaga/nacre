use once_cell::sync::Lazy;
use std::ops::Range;
use tokio::sync::broadcast;

/// An error or warning report.
///
/// Text in reports sent by the library
/// will enclose code in backticks.
#[derive(Clone)]
pub struct Report {
    /// Whether the report corresponds to an error. If false, it is a warning.
    pub is_error: bool,
    /// Where in the code the reported issue occurs.
    pub offset: usize,
    /// Message to show to the user.
    pub message: String,
    /// A further note.
    pub note: Option<String>,
    /// A help suggestion.
    pub help: Option<String>,
    /// Any number of labels with a location in the code and a message each.
    pub labels: Vec<(Range<usize>, String)>,
}

/// A global broadcast channel for [Report].
///
/// Reports will be asynchronously sent through this channel
/// while any operation is ongoing.
pub static REPORTS: Lazy<broadcast::Sender<Report>> = Lazy::new(|| {
    let (r, _) = broadcast::channel(32);
    r
});

pub(crate) fn send(report: Report) {
    let _ = REPORTS.send(report);
}
