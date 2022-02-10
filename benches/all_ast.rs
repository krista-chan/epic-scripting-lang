use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use std::fs;
use scripting_lang::parser::ast::TopLevel;

fn test_all(input: &str) {
    input.parse::<TopLevel>().unwrap();
}

fn criterion_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("src/tests/test.file").unwrap();

    let mut bmg = c.benchmark_group("main");
    bmg.throughput(Throughput::Bytes(input.len() as u64));
    bmg.bench_function("test_all", |b| b.iter(|| test_all(&input)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
