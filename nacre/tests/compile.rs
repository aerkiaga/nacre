use std::fs;
use std::process::Command;

async fn test_compile_file(name: &str) {
    let ir = nacre_compiler::compile(vec![format!("tests::compile::{name}::test")])
        .await
        .unwrap();
    ir.emit_code().expect("Compilation failed");
    fs::remove_file("out.s").unwrap();
    let objcopy_status = Command::new("objcopy")
        .arg("--redefine-sym")
        .arg(format!("tests::compile::{name}::test=test"))
        .arg("out.o")
        .status()
        .expect("Failed to run objcopy");
    assert!(objcopy_status.success(), "objcopy returned error status");
    let cc_status = Command::new("cc")
        .arg("-o")
        .arg("test")
        .arg(format!("tests/compile/{name}.c"))
        .arg("out.o")
        .status()
        .expect("Failed to run cc");
    fs::remove_file("out.o").unwrap();
    assert!(cc_status.success(), "cc returned error status");
    let test_status = Command::new("./test")
        .status()
        .expect("Failed to run the compiled test program");
    fs::remove_file("test").unwrap();
    assert!(test_status.success(), "Test program returned error status");
}

/*#[tokio::test(flavor = "multi_thread")]
async fn test_compile_not() {
    test_compile_file("not").await;
}*/

#[tokio::test(flavor = "multi_thread")]
async fn test_compile_and() {
    test_compile_file("and").await;
}
