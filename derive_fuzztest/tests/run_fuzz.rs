//! Runs the fuzzers with `cargo fuzz run`

#![allow(clippy::unwrap_used)]

use std::path::PathBuf;

use xshell::cmd;

// Windows fuzz runs currently fail with STATUS_DLL_NOT_FOUND
#[cfg(not(target_family = "windows"))]
#[test]
fn run_fuzzers() -> anyhow::Result<()> {
    let sh = xshell::Shell::new()?;
    sh.change_dir(PathBuf::from(file!()).parent().unwrap().parent().unwrap().parent().unwrap().join("fuzz"));
    cmd!(sh, "cargo +nightly fuzz run arbitrary -- -runs=1000").run()?;
    cmd!(sh, "cargo +nightly fuzz run discard_result -- -runs=1000").run()?;
    cmd!(sh, "cargo +nightly fuzz run integer_add -- -runs=1000").run()?;
    cmd!(sh, "cargo +nightly fuzz run strange_argument_patterns -- -runs=1000").run()?;
    Ok(())
}