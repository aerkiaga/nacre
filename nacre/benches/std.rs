const LOGICAL_PATHS: &[&str] = &[
    "std::axioms::ExcludedMiddle",
    "std::axioms::StreicherK",
    "std::axioms::UniquenessOfIdentityProofs",
    "std::axioms::UniquenessOfReflexivityProofs",
    "std::axioms::UniquenessOfReflexivityProofs::from_streicher_k",
    "std::logic::And",
    "std::logic::And::from_both",
    "std::logic::And::left",
    "std::logic::And::right",
    "std::logic::And::symmetric",
    "std::logic::And::transitive",
    "std::logic::Eq::apply_forwards",
    "std::logic::Eq::apply_backwards",
    "std::logic::Eq::reflexive",
    "std::logic::Eq::symmetric",
    "std::logic::Eq::transitive",
    "std::logic::Exists",
    "std::logic::Exists::from_example",
    "std::logic::False",
    "std::logic::If",
    "std::logic::If::reflexive",
    "std::logic::If::transitive",
    "std::logic::Iff",
    "std::logic::Iff::backwards",
    "std::logic::Iff::forwards",
    "std::logic::Iff::from_if",
    "std::logic::Iff::reflexive",
    "std::logic::Iff::symmetric",
    "std::logic::Iff::transitive",
    "std::logic::Not",
    "std::logic::Or",
    "std::logic::Or::from_left",
    "std::logic::Or::from_right",
];

async fn bench_std() {
    for logical_path in LOGICAL_PATHS {
        let _ = nacre_parser::verify(logical_path).await;
    }
}

use criterion::{Criterion, criterion_group, criterion_main};

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("std", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| bench_std())
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
