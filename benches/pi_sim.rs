use std::process::Command;
use std::process::Stdio;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};

use basic_rs::*;

fn compile_basic_program(prog: &str) -> VM {
    let scanner = Scanner::new(prog);
    let ast = Parser::new(scanner).parse().unwrap();

    let ir = compile(&ast).expect("it should compile successfuly");
    let vm = codegen(&ir).expect("it should generate vm code");

    vm
}

fn run_basic_program(vm: &mut VM) {
    let mut printed = Vec::new();
    let mut rng = SmallRng::from_seed([123; 16]);
    vm.run(&mut printed, &mut rng).expect("no runtime error");
}

fn pi_native(n: u64) -> f64 {
    let mut inside = 0;
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
// BASIC compiled to rust by basic2rs crate
// with runtime stripped
fn pi_rs_compiled(n: f64) {
    let mut rng = SmallRng::from_seed([1; 16]);

    let mut data: Vec<f64> = vec![];
    let mut __main__ = || {
        let mut __label__: usize;
        let mut local_0: f64;
        let mut local_1: f64;
        let mut local_2: f64;
        let mut local_3: f64;
        let mut local_4: f64;
        let mut local_5: f64;
        let mut local_6: f64;
        let mut local_7: f64;
        local_1 = 0f64;
        local_2 = 0f64;
        local_3 = 0f64;
        local_4 = 0f64;
        local_5 = 0f64;
        local_6 = 0f64;
        local_7 = 0f64;
        local_3 = 0f64;
        local_2 = n;
        local_0 = local_2;
        local_1 = 1f64;
        'a2: loop {
            if ((local_1 - local_0) > 0f64) {
                __label__ = 6;
                break 'a2;
            } else {
                __label__ = 3;
            }
            local_7 = rng.gen();
            local_5 = rng.gen();
            local_4 = ((local_7 * local_7) + (local_5 * local_5));
            if (local_4 > 1f64) {
                __label__ = 5;
            } else {
                __label__ = 4;
            }
            'a5: loop {
                {
                    if (__label__ == 4) {
                        local_3 = (local_3 + 1f64);
                        __label__ = 5;
                        break 'a5;
                    }
                }
                break;
            }
            local_1 = (local_1 + 1f64);
            __label__ = 2;
            continue 'a2;
        }
        local_6 = ((4f64 * local_3) / local_2);
        return;
    };
    __main__();
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

fn create_compiled_js_command() -> Command {
    let mut command = Command::new("node");
    command.arg("benches/pi_compiled.js");

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

fn bench_pi_compiled_js(c: &mut Criterion) {
    let has_node = Command::new("node")
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .output()
        .is_ok();

    if has_node {
        c.bench_program("bas:compiled_js", create_compiled_js_command());
    }
}

fn bench_pi_rs(c: &mut Criterion) {
    c.bench_function("rust:pi", |b| {
        b.iter(|| {
            let n = black_box(1000);
            pi_native(n);
        });
    });
}

fn bench_pi_rs_compiled(c: &mut Criterion) {
    c.bench_function("bas:compiled_rust", |b| {
        b.iter(|| {
            let n = black_box(1000.0);
            pi_rs_compiled(n);
        });
    });
}

criterion_group!(
    pi_benches,
    bench_pi_bas,
    bench_pi_bas_bytecode,
    bench_pi_py,
    bench_pi_node,
    bench_pi_rs,
    bench_pi_rs_compiled,
    bench_pi_compiled_js
);

criterion_main!(pi_benches);
