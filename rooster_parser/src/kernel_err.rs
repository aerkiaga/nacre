use crate::*;

use rooster_kernel::Context;
use rooster_kernel::Environment;
use rooster_kernel::Error;
use rooster_kernel::Meta;
use rooster_kernel::Term;
use rooster_kernel::TermInner;
use std::ops::Range;

pub(crate) struct TermMeta {
    pub(crate) range: Range<usize>,
    pub(crate) name: Option<String>,
}

impl Default for TermMeta {
    fn default() -> TermMeta {
        TermMeta {
            range: 0..0,
            name: None,
        }
    }
}

impl Meta for TermMeta {}

fn get_forall_type(t: &Term<TermMeta>) -> &Term<TermMeta> {
    if let TermInner::Forall(l, r) = &t.inner {
        l
    } else {
        unreachable!();
    }
}

fn list_forall_params(t: &Term<TermMeta>) -> Vec<&Term<TermMeta>> {
    let mut rv = vec![t];
    match &t.inner {
        TermInner::Forall(l, r) => {
            let mut r2 = list_forall_params(&r);
            rv.append(&mut r2);
        }
        _ => {}
    }
    rv
}

fn adjust_strings(s1: String, s2: String) -> (String, String) {
    let mut l1 = 0;
    for ch in s1.chars() {
        if ch != '`' {
            l1 += 1;
        }
    }
    let mut l2 = 0;
    for ch in s2.chars() {
        if ch != '`' {
            l2 += 1;
        }
    }
    let l = l1.max(l2);
    (format!("{:<1$}", s1, l), format!("{:<1$}", s2, l))
}

fn produce_term(t: &Term<TermMeta>) -> String {
    let name = match &t.meta.name {
        Some(s) => s.clone(),
        None => "_".to_string(),
    };
    match &t.inner {
        TermInner::Prop => "Type".to_string(),
        TermInner::Type(n) => format!("Type{}", n + 1),
        TermInner::Global(_) | TermInner::Variable(_) => name,
        TermInner::Forall(l, r) => {
            format!(
                "type({}: {}) -> {}",
                name,
                produce_term(&*l),
                produce_term(&*r)
            )
        }
        _ => "@".to_string(),
    }
}

#[derive(PartialEq)]
enum ComparisonResult {
    Equal,
    Similar,
    Different,
}

impl ComparisonResult {
    fn is_equal(&self) -> bool {
        match self {
            ComparisonResult::Equal => true,
            _ => false,
        }
    }
}

impl PartialOrd for ComparisonResult {
    fn partial_cmp(&self, other: &ComparisonResult) -> Option<std::cmp::Ordering> {
        Some(match self {
            ComparisonResult::Equal => match other {
                ComparisonResult::Equal => std::cmp::Ordering::Equal,
                _ => std::cmp::Ordering::Greater,
            },
            ComparisonResult::Similar => match other {
                ComparisonResult::Equal => std::cmp::Ordering::Less,
                ComparisonResult::Similar => std::cmp::Ordering::Equal,
                ComparisonResult::Different => std::cmp::Ordering::Greater,
            },
            ComparisonResult::Different => match other {
                ComparisonResult::Different => std::cmp::Ordering::Equal,
                _ => std::cmp::Ordering::Less,
            },
        })
    }
}

fn levenshtein_matrix(
    v1: &Vec<&Term<TermMeta>>,
    v2: &Vec<&Term<TermMeta>>,
    env: &Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
) -> Vec<Vec<usize>> {
    // Create empty matrix
    let mut r = vec![];
    for i in 0..v1.len() + 1 {
        let mut rr = vec![];
        for j in 0..v2.len() + 1 {
            rr.push(0);
        }
        r.push(rr);
    }
    // Initialize trivial prefixes
    for i in 0..v1.len() + 1 {
        r[i][0] = i;
    }
    for j in 0..v2.len() + 1 {
        r[0][j] = j;
    }
    // Compute Levenshtein distances for all prefixes
    for j in 1..v2.len() + 1 {
        for i in 1..v1.len() + 1 {
            // TODO: update context appropriately if possible
            let t1 = get_forall_type(&v1[i - 1]);
            let t2 = get_forall_type(&v2[j - 1]);
            let equal = produce_comparison_rec(t1, t2, env, ctx).1.is_equal();
            let cost = if equal { 0 } else { 1 };
            let del = r[i - 1][j] + 1;
            let ins = r[i][j - 1] + 1;
            let sub = r[i - 1][j - 1] + cost;
            r[i][j] = del.min(ins).min(sub);
        }
    }
    r
}

fn levenshtein_compare<'a>(
    v1: Vec<&'a Term<TermMeta>>,
    v2: Vec<&'a Term<TermMeta>>,
    env: &Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
) -> (
    Vec<Option<&'a Term<TermMeta>>>,
    Vec<Option<&'a Term<TermMeta>>>,
) {
    // Get levenshtein distances between all prefixes
    let matrix = levenshtein_matrix(&v1, &v2, env, ctx);
    // Append elements one by one
    let mut i = v1.len();
    let mut j = v2.len();
    let mut r1 = vec![];
    let mut r2 = vec![];
    while i != 0 && j != 0 {
        let del = matrix[i - 1][j];
        let ins = matrix[i][j - 1];
        let sub = matrix[i - 1][j - 1];
        if del < sub {
            i -= 1;
            r1.push(Some(v1[i]));
            r2.push(None);
        } else if ins < sub {
            j -= 1;
            r1.push(None);
            r2.push(Some(v2[j]));
        } else {
            i -= 1;
            j -= 1;
            r1.push(Some(v1[i]));
            r2.push(Some(v2[j]));
        }
    }
    while i != 0 {
        i -= 1;
        r1.push(Some(v1[i]));
        r2.push(None);
    }
    while j != 0 {
        j -= 1;
        r1.push(None);
        r2.push(Some(v2[j]));
    }
    // Return arrays with gaps
    r1.reverse();
    r2.reverse();
    (r1, r2)
}

fn produce_comparison_rec(
    t1: &Term<TermMeta>,
    t2: &Term<TermMeta>,
    env: &Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
) -> ((String, String), ComparisonResult) {
    // same subterm
    if t1 == t2 {
        let (s1, s2) = adjust_strings(produce_term(t1), produce_term(t2));
        return (
            if s1.len() > 16 {
                ("...".to_string(), "...".to_string())
            } else {
                (s1, s2)
            },
            ComparisonResult::Equal,
        );
    }
    // try to expand
    match t1.inner {
        TermInner::Global(g) => match env.delta_replacement(g) {
            Ok(ct1) => return produce_comparison_rec(&ct1, t2, env, ctx),
            Err(_) => {}
        },
        TermInner::Variable(v) => match ctx.delta_replacement(v) {
            Ok(ct1) => return produce_comparison_rec(&ct1, t2, env, ctx),
            Err(_) => {}
        },
        _ => {}
    }
    match t2.inner {
        TermInner::Global(g) => match env.delta_replacement(g) {
            Ok(ct2) => return produce_comparison_rec(t1, &ct2, env, ctx),
            Err(_) => {}
        },
        TermInner::Variable(v) => match ctx.delta_replacement(v) {
            Ok(ct2) => return produce_comparison_rec(t1, &ct2, env, ctx),
            Err(_) => {}
        },
        _ => {}
    }
    // different terms
    let name1 = match &t1.meta.name {
        Some(s) => s.clone(),
        None => "_".to_string(),
    };
    let name2 = match &t2.meta.name {
        Some(s) => s.clone(),
        None => "_".to_string(),
    };
    match &t1.inner {
        TermInner::Forall(l1, r1) => {
            if let TermInner::Forall(l2, r2) = &t2.inner {
                // Get two lists of parameters
                let mut params1 = list_forall_params(t1);
                let mut params2 = list_forall_params(t2);
                let mut ret1 = params1.pop().unwrap();
                let mut ret2 = params2.pop().unwrap();
                // Map insertions, replacements and deletions
                let (mut opt_params1, mut opt_params2) =
                    levenshtein_compare(params1, params2, env, ctx);
                // Handle trailing parameters
                let ((rets1, rets2), retres) = produce_comparison_rec(ret1, ret2, env, ctx);
                let mut trailing = false;
                let is_none1 = opt_params1.last().unwrap().is_none();
                let is_none2 = opt_params2.last().unwrap().is_none();
                let mut op_trailing = &mut opt_params1;
                let mut op_other = &mut opt_params2;
                let mut this_ret = &mut ret1;
                let mut other_ret = &ret2;
                if is_none1 {
                    op_trailing = &mut opt_params2;
                    op_other = &mut opt_params1;
                    this_ret = &mut ret2;
                    other_ret = &ret1;
                    trailing = true;
                } else if is_none2 {
                    trailing = true;
                }
                let mut best_res = retres;
                let mut best_n = op_trailing.len();
                if trailing {
                    for n in (0..op_trailing.len()).rev() {
                        if let None = op_other[n] {
                            match &op_trailing[n] {
                                Some(t1) => {
                                    let t2 = other_ret;
                                    let (_, res) = produce_comparison_rec(t1, t2, env, ctx);
                                    if res > best_res {
                                        *this_ret = *t1;
                                        best_n = n;
                                        if res.is_equal() {
                                            break;
                                        }
                                        best_res = res;
                                    }
                                }
                                None => {
                                    break;
                                }
                            }
                        } else {
                            break;
                        }
                    }
                    for _ in best_n..op_trailing.len() {
                        op_trailing.pop();
                        op_other.pop();
                    }
                }
                // Format arguments correctly
                let mut r1 = vec![];
                let mut r2 = vec![];
                for n in 0..opt_params1.len() {
                    match opt_params1[n] {
                        Some(p1) => {
                            let name1 = p1.meta.name.clone().unwrap_or("_".to_string());
                            let t1 = get_forall_type(p1);
                            match opt_params2[n] {
                                Some(p2) => {
                                    let name2 = p2.meta.name.clone().unwrap_or("_".to_string());
                                    let t2 = get_forall_type(p2);
                                    let (cname1, cname2) = adjust_strings(name1, name2);
                                    let ((st1, st2), _) = produce_comparison_rec(t1, t2, env, ctx);
                                    r1.push(format!("{}: {}", cname1, st1));
                                    r2.push(format!("{}: {}", cname1, st2));
                                }
                                None => {
                                    r1.push(format!("`{}: {}`", name1, produce_term(t1)));
                                    r2.push("".to_string());
                                }
                            }
                        }
                        None => match opt_params2[n] {
                            Some(p2) => {
                                let name2 = p2.meta.name.clone().unwrap_or("_".to_string());
                                let t2 = get_forall_type(p2);
                                r1.push("".to_string());
                                r2.push(format!("`{}: {}`", name2, produce_term(t2)));
                            }
                            None => {
                                unreachable!();
                            }
                        },
                    }
                }
                // Handle commas
                let mut add_comma = false;
                for n in (0..r1.len()).rev() {
                    if r1[n].len() > 0 {
                        if add_comma {
                            r1[n].push_str(", ");
                        }
                        add_comma = true;
                    }
                }
                add_comma = false;
                for n in (0..r2.len()).rev() {
                    if r2[n].len() > 0 {
                        if add_comma {
                            r2[n].push_str(", ");
                        }
                        add_comma = true;
                    }
                }
                for n in 0..r2.len() {
                    (r1[n], r2[n]) = adjust_strings(r1[n].clone(), r2[n].clone());
                }
                let ((rets1, rets2), _) = produce_comparison_rec(ret1, ret2, env, ctx);
                let rs1 = format!("type({}) -> {}", r1.join(""), rets1);
                let rs2 = format!("type({}) -> {}", r2.join(""), rets2);
                return ((rs1, rs2), ComparisonResult::Different);
            }
        }
        _ => {}
    }
    return (
        adjust_strings(
            format!("`{}`", produce_term(t1)),
            format!("`{}`", produce_term(t2)),
        ),
        ComparisonResult::Different,
    );
}

fn produce_comparison(
    t1: &Term<TermMeta>,
    t2: &Term<TermMeta>,
    env: &Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
) -> (String, String) {
    let (r, _) = produce_comparison_rec(t1, t2, env, ctx);
    r
}

pub(crate) fn report(error: Error<TermMeta>, filename: &str) {
    match error {
        Error::MismatchedType {
            expected,
            found,
            env,
        } => {
            let mut ctx = Context::new();
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: 0,
                message: "mismatched type in definition".to_string(),
                note: {
                    let (exp, fnd) = produce_comparison(&expected, &found, &env, &mut ctx);
                    Some(format!(
                        "mismatched types\nexpected `{}`\nfound    `{}`",
                        exp, fnd
                    ))
                },
                help: None,
                labels: vec![
                    (
                        expected.meta.range.clone(),
                        "type constrained by this".to_string(),
                    ),
                    (found.meta.range.clone(), "has unexpected type".to_string()),
                ],
            });
        }
        Error::Other => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: 0,
                message: "unknown kernel error".to_string(),
                note: None,
                help: None,
                labels: vec![],
            });
        }
    }
}
