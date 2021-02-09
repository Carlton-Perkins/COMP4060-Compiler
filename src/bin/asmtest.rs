use std::{fs::File, io::Write, os::unix::process::ExitStatusExt};
use std::{
    io::{stderr, stdout, Read},
    process::{Command, Output},
};

use tempfile::tempdir;

fn compile_and_run(prog: &String) -> Result<i32, String> {
    // Create new file with arg string
    let dir = tempdir().expect("Failled to alocate temp dir");
    println!("TempDir: {:?}", dir.as_ref());
    let temp_file = dir.path().join("asm.s");
    let mut file = File::create(temp_file.clone()).expect("Failed to create temp file");
    file.write_all(prog.as_bytes())
        .expect("Failed to write asm to file");
    // Assemble that file
    let asmer = "gcc";
    let temp_file_path = temp_file.as_path().to_str().unwrap();
    let asm_args = &["-c", temp_file_path, "-g"];
    let asm_res = run_cmd_and_print(Command::new(asmer).current_dir(dir.as_ref()).args(asm_args));

    match asm_res.status.code().unwrap() {
        0 => println!("ASM Success"),
        n => {
            println!("ASM Failed with code {}", n);
            stdout().write_all(&asm_res.stdout).unwrap();
            stderr().write_all(&asm_res.stderr).unwrap();
            return Err(format!("ASM Failed with code {}", n));
        }
    }

    // Link that file
    let linker = "gcc";
    let linker_args = &["-o", "asm", "asm.o", "-g"];
    let linker_res = run_cmd_and_print(
        Command::new(linker)
            .current_dir(dir.as_ref())
            .args(linker_args),
    );

    match linker_res.status.code().unwrap() {
        0 => println!("Link Success"),
        n => {
            println!("Link Failed with code {}", n);
            stdout().write_all(&linker_res.stdout).unwrap();
            stderr().write_all(&linker_res.stderr).unwrap();
            return Err(format!("Link Failed with code {}", n));
        }
    }

    // Run the resulting binary
    let run_res = run_cmd_and_print(Command::new("./asm").current_dir(dir.as_ref()));

    match run_res.status.code() {
        Some(n) => Ok(n),
        None => match run_res.status.signal() {
            Some(sign) => Err(format!("Run Failure with sig {}", sign)),
            None => Err("Run Unknown Failure".to_string()),
        },
    }
}
fn run_cmd_and_print(c: &mut Command) -> Output {
    c.output().expect(&format!("CMD {:?} failed", c))
}

fn main() {
    // println!("{:?}", std::env::current_dir().unwrap());
    let input_file = "src/bin/asmtest3.s";
    // let output_file = "src/bin/asmtest.o";
    // let bin_file = "src/bin/asmtest";
    // let asmer = "nasm";
    // let linker = "ld";
    // let asmformat = "elf64";
    // let output = Command::new(asmer)
    //     .args(&["-f", asmformat, "-w+all", "-o", output_file, input_file])
    //     .output()
    //     .unwrap();

    // println!("status: {}", output.status);
    // stdout().write_all(&output.stdout).unwrap();
    // stderr().write_all(&output.stderr).unwrap();

    // let output = Command::new(linker)
    //     .args(&["-o", bin_file, output_file])
    //     .output()
    //     .unwrap();

    // println!("status: {}", output.status);
    // stdout().write_all(&output.stdout).unwrap();
    // stderr().write_all(&output.stderr).unwrap();

    // let output = Command::new(bin_file).output().unwrap();

    // println!("status: {}", output.status);
    // stdout().write_all(&output.stdout).unwrap();
    // stderr().write_all(&output.stderr).unwrap();

    let mut file = File::open(input_file).expect("No input file");
    let mut con = String::new();
    file.read_to_string(&mut con).expect("No content");
    let res = compile_and_run(&con).expect("Failed to compile and run");
    println!("Result: {}", res);
}
