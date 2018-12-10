use std::process::Command;
use std::process::Stdio;

use criterion::{criterion_group, criterion_main, Criterion};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};

use basic_rs::*;

fn compile_basic_program(prog: &str) -> VM {
    let scanner = Scanner::new(prog);
    let ast = Parser::new(scanner).parse().unwrap();

    let vm = compile(&ast).expect("it should compile successfuly");

    vm
}

fn run_basic_program(vm: &mut VM) {
    let mut printed = Vec::new();
    let mut rng = SmallRng::from_seed([123; 16]);
    vm.run(&mut printed, &mut rng).expect("no runtime error");
}

fn pi_native() -> f64 {
    let mut inside = 0;
    let n = 1000;
    let mut rng = SmallRng::from_seed([1; 16]);
    for _ in 0..n {
        let x: f64 = rng.gen();
        let y: f64 = rng.gen();
        let d = x * x + y * y;
        if d < 1.0 {
            inside += 1;
        }
    }
    let pi = 4.0 * (inside as f64) / (n as f64);
    pi
}

fn create_python_command() -> Command {
    let mut command = Command::new("python3");
    command.arg("benches/pi.py").arg("1000");

    command
}

fn create_js_command() -> Command {
    let mut command = Command::new("node");
    command.arg("benches/pi.js");

    command
}

fn bench_pi_bas(c: &mut Criterion) {
    c.bench_function("interpreter:pi.bas", |b| {
        b.iter(|| {
            let mut vm = compile_basic_program(include_str!(
                "../sample_programs/pi.bas"
            ));
            run_basic_program(&mut vm);
        });
    });
}

fn bench_pi_bas_bytecode(c: &mut Criterion) {
    let mut vm =
        compile_basic_program(include_str!("../sample_programs/pi.bas"));

    c.bench_function("interpreter:pi.bas (bytecode)", move |b| {
        b.iter(|| {
            vm.reset();
            run_basic_program(&mut vm);
        });
    });
}

fn bench_pi_py(c: &mut Criterion) {
    let has_python3 = Command::new("python3")
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .output()
        .is_ok();

    if has_python3 {
        c.bench_program("extern:pi.py", create_python_command());
    }
}

fn bench_pi_node(c: &mut Criterion) {
    let has_node = Command::new("node")
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .output()
        .is_ok();

    if has_node {
        c.bench_program("extern:pi.js", create_js_command());
    }
}

fn bench_pi_rs(c: &mut Criterion) {
    c.bench_function("rust:pi", |b| {
        b.iter(|| {
            pi_native();
        });
    });
}

criterion_group!(basic, bench_pi_bas);
criterion_group!(basic_compiled, bench_pi_bas_bytecode);
criterion_group!(python, bench_pi_py);
criterion_group!(js, bench_pi_node);
criterion_group!(rust, bench_pi_rs);

criterion_main!(basic, basic_compiled, python, js, rust);
