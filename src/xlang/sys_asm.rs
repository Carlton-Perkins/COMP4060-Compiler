use crate::common::{traits::Emit, types::Number};
use std::{
    fs::File,
    io::Write,
    os::unix::process::ExitStatusExt,
    path::Path,
    process::{Command, Output},
};

use tempfile::tempdir;

use super::XProgram;

pub trait CompileAndRun {
    fn run(&self) -> Number;
}

impl CompileAndRun for XProgram {
    fn run(&self) -> Number {
        compile_and_run(&self.emit()).expect("Compile failed") as Number
    }
}

pub fn compile_and_run(prog: &String) -> Result<Number, String> {
    println!("Program: \n{}", prog);

    // Create new file with arg string
    let dir = tempdir().expect("Failed to alocate temp dir");
    println!("TempDir: {:?}", dir.as_ref());
    let temp_file = dir.path().join("asm.s");
    let runtime_path = Path::new("./src/xlang/runtime.c")
        .canonicalize()
        .expect("No runtime found");
    let mut file = File::create(temp_file.clone()).expect("Failed to create temp file");
    file.write_all(prog.as_bytes())
        .expect("Failed to write asm to file");

    // Assemble that file
    let asmer = "gcc";
    let temp_file_path = temp_file.as_path().to_str().unwrap();
    let asm_args = &[
        temp_file_path,
        runtime_path.to_str().unwrap(),
        "-g",
        "-o",
        "asm",
    ];
    let asm_res = run_cmd_and_print(Command::new(asmer).current_dir(dir.as_ref()).args(asm_args));

    match asm_res.status.code().unwrap() {
        0 => println!("ASM Success"),
        n => {
            println!("ASM Failed with code {}", n);
            println!(
                "ASM Stdout: {:?}",
                String::from_utf8_lossy(&asm_res.stdout).to_string()
            );
            println!(
                "ASM Stderr: {:?}",
                String::from_utf8_lossy(&asm_res.stderr).to_string()
            );
            return Err(format!("ASM Failed with code {}", n));
        }
    }

    // Run the resulting binary
    let run_res = run_cmd_and_print(Command::new("./asm").current_dir(dir.as_ref()));

    match run_res.status.code() {
        Some(code) => {
            let ret_str = String::from_utf8_lossy(&run_res.stdout).to_string();
            let ret_str_trimed = ret_str.trim();
            println!("Run Stdout: {:?}", ret_str_trimed);
            let ret: Number = ret_str_trimed.parse().unwrap_or(code.into());
            println!("Run ret code: {:?}", ret);
            Ok(ret)
        }
        None => match run_res.status.signal() {
            Some(sign) => Err(format!("Run Failure with sig {}", sign)),
            None => Err("Run Unknown Failure".to_string()),
        },
    }
}

fn run_cmd_and_print(c: &mut Command) -> Output {
    c.output().expect(&format!("CMD {:?} failed", c))
}
