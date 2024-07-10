#[macro_use]
extern crate afl;

use nacre_kernel::{Environment, Meta, Term, TermInner};
use std::sync::Arc;

struct FuzzMeta {}

impl Meta for FuzzMeta {}

fn create_arbitrary(data: &[u8], start: usize) -> (Term<FuzzMeta>, usize) {
    if start >= data.len() {
        return ((TermInner::Prop, &Arc::new(FuzzMeta {})).into(), start);
    }
    match data[start] % 8 {
        0 => ((TermInner::Prop, &Arc::new(FuzzMeta {})).into(), start + 1),
        1 => (
            (
                TermInner::Type((data[start] / 8).into()),
                &Arc::new(FuzzMeta {}),
            )
                .into(),
            start + 1,
        ),
        2 => (
            (
                TermInner::Global((data[start] / 8).into()),
                &Arc::new(FuzzMeta {}),
            )
                .into(),
            start + 1,
        ),
        3 => (
            (
                TermInner::Variable((data[start] / 8).into()),
                &Arc::new(FuzzMeta {}),
            )
                .into(),
            start + 1,
        ),
        4 => {
            let (t, start2) = create_arbitrary(data, start + 1);
            let (v, start3) = create_arbitrary(data, start2);
            (
                (
                    TermInner::Forall(Box::new(t), Box::new(v)),
                    &Arc::new(FuzzMeta {}),
                )
                    .into(),
                start3,
            )
        }
        5 => {
            let (t, start2) = create_arbitrary(data, start + 1);
            let (v, start3) = create_arbitrary(data, start2);
            (
                (
                    TermInner::Lambda(Box::new(t), Box::new(v)),
                    &Arc::new(FuzzMeta {}),
                )
                    .into(),
                start3,
            )
        }
        6 => {
            let (a, start2) = create_arbitrary(data, start + 1);
            let (b, start3) = create_arbitrary(data, start2);
            (
                (
                    TermInner::Apply(Box::new(a), Box::new(b)),
                    &Arc::new(FuzzMeta {}),
                )
                    .into(),
                start3,
            )
        }
        7 => {
            let (var, start2) = create_arbitrary(data, start + 1);
            let (v, start3) = create_arbitrary(data, start2);
            (
                (
                    TermInner::Let(Box::new(var), Box::new(v)),
                    &Arc::new(FuzzMeta {}),
                )
                    .into(),
                start3,
            )
        }
        _ => unreachable!(),
    }
}

fn main() {
    let term_false = (
        TermInner::Forall(
            Box::new((TermInner::Prop, &Arc::new(FuzzMeta {})).into()),
            Box::new((TermInner::Variable(0), &Arc::new(FuzzMeta {})).into()),
        ),
        &Arc::new(FuzzMeta {}),
    )
        .into();
    fuzz!(|data: &[u8]| {
        if data.len() > 1000 {
            return;
        }
        let mut env = Environment::<FuzzMeta>::from_vec(vec![]);
        let mut start = 0;
        while start < data.len() {
            println!("________________"); //D
            let (term, new_start) = create_arbitrary(data, start);
            println!("Term {:?}", term); //D
            println!("Env\n{:?}", env); //D
            start = new_start;
            if term.well_formed(&env) {
                match term.get_type(&env) {
                    Ok(tt) => {
                        println!("Type {:?}", tt);
                        assert!(tt.well_formed(&env));
                        let ctt = tt.normalize(&env).map_err(|_| ()).unwrap();
                        assert!(ctt != term_false);
                        assert!(env
                            .add_definition(Some(Arc::new(term)), Arc::new(tt))
                            .is_ok());
                    }
                    Err(_) => {
                        println!("Can't compute type");
                    }
                }
            } else {
                println!("Non-WF");
            }
        }
    });
}
