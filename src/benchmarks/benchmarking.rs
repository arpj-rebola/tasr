use std::{
    path::{Path},
};

use criterion::{self, Criterion, criterion_group, criterion_main};

use tasr::{
    app::{PreprocessingConfig, CheckingConfig},
};

fn run_benchmark(path: &str) {
    run_preprocessing(path);
    run_checking(path);
}

fn run_preprocessing(basename: &str) {
    let cnf_string = format!("benches/{}.cnf", basename);
    let raw_string = format!("benches/{}.raw", basename);
    let asr_string = format!("temp/{}.asr", basename);
    let mut pp = PreprocessingConfig {
        cnf: Path::new(&cnf_string).to_path_buf(),
        cnf_binary: false,
        asr: Path::new(&raw_string).to_path_buf(),
        asr_binary: false,
        temp: Path::new("temp/").to_path_buf(),
        output: Some(Path::new(&asr_string).to_path_buf()),
        chunk: 1000000u64,
        stats: true,
        data: None,
        integrity: None,
        preprocessing: None,
    };
    pp.integrity();
    pp.preprocess();
}
fn run_checking(basename: &str) {
    let cnf_string = format!("benches/{}.cnf", basename);
    let asr_string = format!("temp/{}.asr", basename);
    let mut chk = CheckingConfig {
        cnf: Path::new(&cnf_string).to_path_buf(),
        cnf_binary: false,
        asr: Path::new(&asr_string).to_path_buf(),
        asr_binary: false,
        permissive: true,
        select: 0u64,
        parts: 1u64,
        stats: true,
        data: None,
        integrity: None,
        correctness: None,
    };
    chk.integrity();
    chk.correctness();
}

fn main() {
    let list = vec![
        // "3bitadd_32.cnf.gz.CP3-cnfmiter",
        // "6s153",
        // "baseballcover15with25",
        // "dlx1c.ucl.sat.chaff.4.1.bryant",
        // "f6bidw",
        // "fclqcolor-18-14-11.cnf.gz.CP3-cnfmiter",
        // "fclqcolor-20-15-12.cnf.gz.CP3-cnfmiter",
        // "jkkk-one-one-10-30-unsat",
        // "ls15-normalized.cnf.gz.CP3-cnfmiter",
        // "ps_200_300_70",
        // "ps_200_301_70",
        // "ps_200_305_70",
        // "ps_200_306_70",
        // "ps_200_316_70",
        // "ps_200_317_70",
        "Steiner-15-7-bce",
        "Steiner-27-10-bce",
        "Steiner-45-16-bce",
        "Steiner-9-5-bce"
    ];
    for item in &list {
        println!("running {}", item);
        run_benchmark(item);
    }
}