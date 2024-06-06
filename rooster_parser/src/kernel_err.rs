use crate::*;

use rooster_kernel::Error;
use rooster_kernel::Term;

pub(crate) fn report(error: Error, filename: &str) {
    match error {
        Error::MismatchedType => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: 0,
                message: "mismatched type in definition".to_string(),
                note: None,
                help: None,
                labels: vec![],
            });
        }
        Error::Other => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: 0,
                message: "unknown kernel error".to_string(),
                note: None,
                help: None,
                labels: vec![],
            });
        }
    }
}
