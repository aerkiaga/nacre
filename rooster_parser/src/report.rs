use once_cell::sync::Lazy;
use std::ops::Range;
use tokio::sync::broadcast;

#[derive(Clone)]
pub struct Report {
    pub is_error: bool,
    pub offset: usize,
    pub message: String,
    pub note: Option<String>,
    pub help: Option<String>,
    pub labels: Vec<(Range<usize>, String)>,
}

pub static REPORTS: Lazy<broadcast::Sender<Report>> = Lazy::new(|| {
    let (r, _) = broadcast::channel(32);
    r
});

pub(crate) fn send(report: Report) {
    REPORTS.send(report);
}
