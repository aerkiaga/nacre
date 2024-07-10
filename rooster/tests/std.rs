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

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_std() {
    for logical_path in LOGICAL_PATHS {
        rooster_parser::verify(logical_path).await.unwrap();
    }
}
