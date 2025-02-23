use std::collections::HashMap;
use tokio::fs::read_dir;

fn directory_loader(path: &str) -> nacre_cache::LoaderFuture<'_, HashMap<String, bool>> {
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
static DIRECTORY__CACHE: nacre_cache::Cache<HashMap<String, bool>> =
    nacre_cache::Cache::new(directory_loader as _);

// Search the contents of a directory for a name and return whether it exists and is a directory
// If it is a file, the name searched includes extension .na
pub async fn is_directory(directory: &str, name: &str) -> Result<bool, ()> {
    let mut name_na = name.to_string();
    name_na.push_str(".na");
    let table = DIRECTORY__CACHE.get(directory).await.unwrap();
    if let Some(is_dir) = table.get(&name_na) {
        if !is_dir {
            return Ok(false);
        }
    }
    if let Some(is_dir) = table.get(name) {
        if *is_dir {
            return Ok(true);
        }
    }
    Err(())
}

// Gets logical path, returns file path and identifier
pub(crate) async fn get_physical_path(logical_path: &str) -> Result<(String, String), ()> {
    let mut file_path = ".".to_string();
    let mut identifier = "".to_string();
    let mut n = 0;
    let mut found_file = false;
    for component in logical_path.split(':') {
        if n % 2 == 1 {
            if !component.is_empty() {
                panic!();
            }
            if found_file && !identifier.is_empty() {
                identifier.push_str("::");
            }
        } else if !found_file {
            match is_directory(&file_path, component).await {
                Ok(is_dir) => {
                    file_path.push('/');
                    file_path.push_str(component);
                    if !is_dir {
                        file_path.push_str(".na");
                        found_file = true;
                    }
                }
                Err(_) => return Err(()),
            }
        } else {
            identifier.push_str(component);
        }
        n += 1;
    }
    if n % 2 == 0 {
        panic!();
    }
    Ok((file_path, identifier))
}

// Checks whether a certain relative path can be accessed from a file
pub(crate) fn check_path_access(relative_path: &str, _filename: &str) -> bool {
    let mut has_super = false;
    let mut cannot_access = false;
    for (n, component) in relative_path.split(':').enumerate() {
        if n % 2 == 1 {
            if !component.is_empty() {
                panic!();
            }
        } else if component == "super" {
            has_super = true;
        } else if !component.is_empty() && component.chars().collect::<Vec<_>>()[0] == '_' {
            if cannot_access {
                return false;
            }
            if has_super {
                has_super = false;
                cannot_access = true;
            }
        } else if has_super {
            has_super = false;
            cannot_access = true;
        }
    }
    true
}

// Gets absolute path from relative path and file name
pub(crate) fn make_absolute(relative_path: &str, filename: &str) -> String {
    let mut r = filename[..filename.len() - 3]
        .split('/')
        .filter(|x| !x.is_empty() && x != &".")
        .collect::<Vec<_>>();
    let mut n = 0;
    for component in relative_path.split(':') {
        if n % 2 == 1 {
            if !component.is_empty() {
                panic!();
            }
        } else if component == "super" {
            r.pop();
        } else {
            r.push(component);
        }
        n += 1;
    }
    if n % 2 == 0 {
        panic!();
    }
    r.join("::")
}
