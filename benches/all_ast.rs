use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::fs;

fn test_all() {
    let input = fs::read_to_string("test.file").unwrap();
    
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| test_all()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
