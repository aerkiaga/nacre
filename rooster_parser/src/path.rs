use crate::*;

use std::collections::HashMap;
use tokio::fs::read_dir;

fn directory_loader(
    path: &str,
) -> Pin<Box<dyn Future<Output = Result<HashMap<String, bool>, ()>> + Send + '_>> {
    Box::pin(async move {
        let mut rd = read_dir(path).await.unwrap();
        let mut r = HashMap::new();
        loop {
            let entry = match rd.next_entry().await.unwrap() {
                Some(entry) => entry,
                None => break,
            };
            let name = entry.file_name().to_str().unwrap().to_string();
            let is_dir = entry.file_type().await.unwrap().is_dir();
            r.insert(name, is_dir);
        }
        Ok(r)
    })
}

// Stores files and directories with name under each directory
// Each entry has a true value if it is a directory
static DIRECTORY__CACHE: cache::Cache<HashMap<String, bool>> =
    cache::Cache::new(directory_loader as _);

// Search the contents of a directory for a name and return whether it exists and is a directory
// If it is a file, the name searched includes extension .roo
pub async fn is_directory(directory: &str, name: &str) -> Result<bool, ()> {
    let mut name_roo = name.to_string();
    name_roo.push_str(".roo");
    let table = DIRECTORY__CACHE.get(directory).await.unwrap();
    match table.get(&name_roo) {
        Some(is_dir) => {
            if !is_dir {
                return Ok(false);
            }
        }
        None => {}
    }
    match table.get(name) {
        Some(is_dir) => {
            if *is_dir {
                return Ok(true);
            }
        }
        None => {}
    }
    Err(())
}

// Gets logical path, returns file path and identifier
pub(crate) async fn get_physical_path(logical_path: &str) -> (String, String) {
    let mut file_path = ".".to_string();
    let mut identifier = "".to_string();
    let mut n = 0;
    let mut found_file = false;
    for component in logical_path.split(':') {
        if n % 2 == 1 {
            if component != "" {
                panic!();
            }
            if found_file && identifier.len() > 0 {
                identifier.push_str("::");
            }
        } else {
            if !found_file {
                match is_directory(&file_path, component).await {
                    Ok(is_dir) => {
                        file_path.push('/');
                        file_path.push_str(component);
                        if !is_dir {
                            file_path.push_str(".roo");
                            found_file = true;
                        }
                    }
                    Err(_) => {
                        panic!();
                    }
                }
            } else {
                identifier.push_str(component);
            }
        }
        n += 1;
    }
    if n % 2 == 0 {
        panic!();
    }
    (file_path, identifier)
}

// Gets absolute path from relative path and file name
pub(crate) fn make_absolute(relative_path: &str, filename: &str) -> String {
    filename[..filename.len() - 4]
        .split('/')
        .into_iter()
        .filter(|x| x.len() > 0 && x != &".")
        .collect::<Vec<_>>()
        .join("::")
        + "::"
        + relative_path
}
