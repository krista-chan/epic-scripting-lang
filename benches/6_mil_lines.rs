use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use scripting_lang::parser::ast::TopLevel;

fn six_million(input: &str) {
    input.parse::<TopLevel>().unwrap();
}

fn criterion_benchmark(c: &mut Criterion) {
    let input = "const uwu = {owo: :uwu}\n".repeat(6e6 as usize);

    let mut bmg = c.benchmark_group("main");
    bmg.throughput(Throughput::Bytes(input.len() as u64));
    bmg.bench_function("six_million", |b| b.iter(|| six_million(&input)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
