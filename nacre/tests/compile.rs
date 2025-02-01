use std::process::Command;
use std::fs;

#[tokio::test(flavor = "multi_thread")]
async fn test_compile() {
    nacre_parser::verify("tests::compile::test::a").await.unwrap();
    let ir = nacre_compiler::compile(vec!["tests::compile::test::a".to_string()]).await.unwrap();
    ir.emit_code();
    fs::remove_file("out.s").unwrap();
    let objcopy_status = Command::new("objcopy")
        .arg("--redefine-sym")
        .arg("tests::compile::test::a=test_a")
        .arg("out.o")
        .status()
        .expect("Failed to run objcopy");
    assert!(objcopy_status.success(), "objcopy returned error status");
    let cc_status = Command::new("cc")
        .arg("-o")
        .arg("test")
        .arg("tests/compile/test.c")
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
