use crate::*;

use nacre_kernel::Context;
use nacre_kernel::Environment;
use nacre_kernel::Error;
use nacre_kernel::Meta;
use nacre_kernel::Term;
use nacre_kernel::TermInner;
use std::ops::Range;

pub(crate) struct TermMeta {
    pub(crate) range: Range<usize>,
    pub(crate) name: Option<String>,
    pub(crate) filename: String,
}

impl Default for TermMeta {
    fn default() -> TermMeta {
        TermMeta {
            range: 0..0,
            name: None,
            filename: "".to_string(),
        }
    }
}

impl Meta for TermMeta {}

fn get_type(t: &Term<TermMeta>) -> &Term<TermMeta> {
    if let TermInner::Forall(l, _r) = &t.inner {
        l
    } else if let TermInner::Lambda(l, _r) = &t.inner {
        l
    } else {
        unreachable!();
    }
}

fn eq_weak(a: &Term<TermMeta>, b: &Term<TermMeta>, level: usize) -> bool {
    match &a.inner {
        TermInner::Prop => matches!(b.inner, TermInner::Prop),
        TermInner::Type(n1) => {
            if let TermInner::Type(n2) = b.inner {
                *n1 == n2
            } else {
                false
            }
        }
        TermInner::Global(g1) => {
            if let TermInner::Global(g2) = b.inner {
                *g1 == g2
            } else {
                false
            }
        }
        TermInner::Variable(v1) => {
            if let TermInner::Variable(v2) = b.inner {
                if *v1 < level {
                    *v1 == v2
                } else {
                    a.meta.name == b.meta.name
                }
            } else {
                false
            }
        }
        TermInner::Forall(l1, r1) => {
            if let TermInner::Forall(l2, r2) = &b.inner {
                eq_weak(l1, l2, level) && eq_weak(r1, r2, level + 1)
            } else {
                false
            }
        }
        TermInner::Lambda(l1, r1) => {
            if let TermInner::Lambda(l2, r2) = &b.inner {
                eq_weak(l1, l2, level) && eq_weak(r1, r2, level + 1)
            } else {
                false
            }
        }
        TermInner::Apply(l1, r1) => {
            if let TermInner::Apply(l2, r2) = &b.inner {
                eq_weak(l1, l2, level) && eq_weak(r1, r2, level)
            } else {
                false
            }
        }
        TermInner::Let(l1, r1) => {
            if let TermInner::Let(l2, r2) = &b.inner {
                eq_weak(l1, l2, level) && eq_weak(r1, r2, level + 1)
            } else {
                false
            }
        }
    }
}

fn list_params(t: &Term<TermMeta>, is_lambda: bool) -> Vec<&Term<TermMeta>> {
    let mut rv = vec![t];
    if is_lambda {
        if let TermInner::Lambda(_l, r) = &t.inner {
            let mut r2 = list_params(r, is_lambda);
            rv.append(&mut r2);
        }
    } else if let TermInner::Forall(_l, r) = &t.inner {
        let mut r2 = list_params(r, is_lambda);
        rv.append(&mut r2);
    }
    rv
}

fn adjust_strings(s1: String, s2: String) -> (String, String) {
    let mut l1 = 0;
    let mut fl1 = 0;
    for ch in s1.chars() {
        if ch == '`' {
            fl1 += 1;
        }
        l1 += 1;
    }
    let mut l2 = 0;
    let mut fl2 = 0;
    for ch in s2.chars() {
        if ch == '`' {
            fl2 += 1;
        }
        l2 += 1;
    }
    let l = (l1 - fl1).max(l2 - fl2);
    (
        format!("{:<1$}", s1, l + fl1),
        format!("{:<1$}", s2, l + fl2),
    )
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
            format!("type({}: {}) -> {}", name, produce_term(l), produce_term(r))
        }
        TermInner::Lambda(l, r) => {
            format!("fn({}: {}) {{{}}}", name, produce_term(l), produce_term(r))
        }
        TermInner::Apply(l, r) => {
            format!("({} {})", produce_term(l), produce_term(r))
        }
        TermInner::Let(l, r) => {
            format!("let {} = {}; {}", name, produce_term(l), produce_term(r))
        }
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
        matches!(self, ComparisonResult::Equal)
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
    v1: &[&Term<TermMeta>],
    v2: &[&Term<TermMeta>],
    env: &Environment<TermMeta>,
    ctx1: &mut Context<TermMeta>,
    ctx2: &mut Context<TermMeta>,
) -> Vec<Vec<usize>> {
    // Create empty matrix
    let mut r = vec![];
    for _ in 0..v1.len() + 1 {
        let rr = vec![0; v2.len() + 1];
        r.push(rr);
    }
    // Initialize trivial prefixes
    #[allow(clippy::needless_range_loop)]
    for i in 0..v1.len() + 1 {
        r[i][0] = i;
    }
    for j in 0..v2.len() + 1 {
        r[0][j] = j;
    }
    // Compute Levenshtein distances for all prefixes
    for j in 1..v2.len() + 1 {
        for i in 1..v1.len() + 1 {
            let t1 = get_type(v1[i - 1]);
            let t2 = get_type(v2[j - 1]);
            for _ in 0..i - 1 {
                ctx1.add_inner(
                    None,
                    (TermInner::Prop, &Arc::new(TermMeta::default())).into(),
                );
            }
            for _ in 0..j - 1 {
                ctx2.add_inner(
                    None,
                    (TermInner::Prop, &Arc::new(TermMeta::default())).into(),
                );
            }
            let equal = produce_comparison_rec(t1, t2, env, ctx1, ctx2, true)
                .1
                .is_equal();
            for _ in 0..i - 1 {
                ctx1.remove_inner();
            }
            for _ in 0..j - 1 {
                ctx2.remove_inner();
            }
            let cost = if equal { 0 } else { 1 };
            let del = r[i - 1][j] + 1;
            let ins = r[i][j - 1] + 1;
            let sub = r[i - 1][j - 1] + cost;
            r[i][j] = del.min(ins).min(sub);
        }
    }
    r
}

type ParamsWithGaps<'a> = Vec<Option<&'a Term<TermMeta>>>;

fn levenshtein_compare<'a>(
    v1: Vec<&'a Term<TermMeta>>,
    v2: Vec<&'a Term<TermMeta>>,
    env: &Environment<TermMeta>,
    ctx1: &mut Context<TermMeta>,
    ctx2: &mut Context<TermMeta>,
) -> (ParamsWithGaps<'a>, ParamsWithGaps<'a>) {
    // Get levenshtein distances between all prefixes
    let matrix = levenshtein_matrix(&v1, &v2, env, ctx1, ctx2);
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

fn handle_prototype(
    t1: &Term<TermMeta>,
    t2: &Term<TermMeta>,
    env: &Environment<TermMeta>,
    ctx1: &mut Context<TermMeta>,
    ctx2: &mut Context<TermMeta>,
    is_lambda: bool,
) -> ((String, String), ComparisonResult) {
    // Get two lists of parameters
    let mut params1 = list_params(t1, is_lambda);
    let mut params2 = list_params(t2, is_lambda);
    let nparam1 = params1.len();
    let nparam2 = params2.len();
    let mut ret1 = params1.pop().unwrap();
    let mut ret2 = params2.pop().unwrap();
    // Map insertions, replacements and deletions
    let (mut opt_params1, mut opt_params2) = levenshtein_compare(params1, params2, env, ctx1, ctx2);
    // Handle trailing parameters
    for _ in 0..nparam1 {
        ctx1.add_inner(
            None,
            (TermInner::Prop, &Arc::new(TermMeta::default())).into(),
        );
    }
    for _ in 0..nparam2 {
        ctx2.add_inner(
            None,
            (TermInner::Prop, &Arc::new(TermMeta::default())).into(),
        );
    }
    let (_, retres) = produce_comparison_rec(ret1, ret2, env, ctx1, ctx2, false);
    for _ in 0..nparam1 {
        ctx1.remove_inner();
    }
    for _ in 0..nparam2 {
        ctx2.remove_inner();
    }
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
            if op_other[n].is_none() {
                match &op_trailing[n] {
                    Some(t1) => {
                        let t2 = other_ret;
                        let (_, res) = produce_comparison_rec(t1, t2, env, ctx1, ctx2, false);
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
    let mut n1 = 0;
    let mut n2 = 0;
    let mut at_least_one_equal = false;
    for n in 0..opt_params1.len() {
        match opt_params1[n] {
            Some(p1) => {
                let name1 = p1.meta.name.clone().unwrap_or("_".to_string());
                let t1 = get_type(p1);
                match opt_params2[n] {
                    Some(p2) => {
                        let name2 = p2.meta.name.clone().unwrap_or("_".to_string());
                        let t2 = get_type(p2);
                        let (cname1, cname2) = adjust_strings(name1, name2);
                        for _ in 0..n1 {
                            ctx1.add_inner(
                                None,
                                (TermInner::Prop, &Arc::new(TermMeta::default())).into(),
                            );
                        }
                        for _ in 0..n2 {
                            ctx2.add_inner(
                                None,
                                (TermInner::Prop, &Arc::new(TermMeta::default())).into(),
                            );
                        }
                        let ((st1, st2), res) =
                            produce_comparison_rec(t1, t2, env, ctx1, ctx2, false);
                        if res.is_equal() {
                            at_least_one_equal = true;
                        }
                        for _ in 0..n1 {
                            ctx1.remove_inner();
                        }
                        for _ in 0..n2 {
                            ctx2.remove_inner();
                        }
                        r1.push(format!("{}: {}", cname1, st1));
                        r2.push(format!("{}: {}", cname2, st2));
                        n2 += 1;
                    }
                    None => {
                        r1.push(format!("`{}: {}`", name1, produce_term(t1)));
                        r2.push("".to_string());
                    }
                }
                n1 += 1;
            }
            None => match opt_params2[n] {
                Some(p2) => {
                    let name2 = p2.meta.name.clone().unwrap_or("_".to_string());
                    let t2 = get_type(p2);
                    r1.push("".to_string());
                    r2.push(format!("`{}: {}`", name2, produce_term(t2)));
                    n2 += 1;
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
        if !r1[n].is_empty() {
            if add_comma {
                r1[n].push_str(", ");
            }
            add_comma = true;
        }
    }
    add_comma = false;
    for n in (0..r2.len()).rev() {
        if !r2[n].is_empty() {
            if add_comma {
                r2[n].push_str(", ");
            }
            add_comma = true;
        }
    }
    for n in 0..r2.len() {
        (r1[n], r2[n]) = adjust_strings(r1[n].clone(), r2[n].clone());
    }
    let ((rets1, rets2), _) = produce_comparison_rec(ret1, ret2, env, ctx1, ctx2, false);
    let rs1 = format!("type({}) -> {}", r1.join(""), rets1);
    let rs2 = format!("type({}) -> {}", r2.join(""), rets2);
    (
        (rs1, rs2),
        if at_least_one_equal {
            ComparisonResult::Similar
        } else {
            ComparisonResult::Different
        },
    )
}

fn produce_comparison_rec(
    t1: &Term<TermMeta>,
    t2: &Term<TermMeta>,
    env: &Environment<TermMeta>,
    ctx1: &mut Context<TermMeta>,
    ctx2: &mut Context<TermMeta>,
    simplified: bool,
) -> ((String, String), ComparisonResult) {
    // same subterm
    if eq_weak(t1, t2, 0) {
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
    // different terms
    if !simplified {
        match &t1.inner {
            TermInner::Forall(_, _) => {
                if let TermInner::Forall(_, _) = &t2.inner {
                    return handle_prototype(t1, t2, env, ctx1, ctx2, false);
                }
            }
            TermInner::Lambda(_, _) => {
                if let TermInner::Lambda(_, _) = &t2.inner {
                    return handle_prototype(t1, t2, env, ctx1, ctx2, true);
                }
            }
            _ => {}
        }
    }
    // try to expand
    match &t1.inner {
        TermInner::Global(g) => {
            if let Ok(ct1) = env.delta_replacement(*g) {
                let (s12, res) = produce_comparison_rec(&ct1, t2, env, ctx1, ctx2, false);
                if res > ComparisonResult::Different {
                    return (s12, res);
                }
            }
        }
        TermInner::Variable(v) => {
            if let Ok(ct1) = ctx1.delta_replacement(*v) {
                let (s12, res) = produce_comparison_rec(&ct1, t2, env, ctx1, ctx2, false);
                if res > ComparisonResult::Different {
                    return (s12, res);
                }
            }
        }
        TermInner::Apply(l, r) => match &l.inner {
            TermInner::Lambda(_ll, lr) => {
                if let Ok(ct1) = lr.replace_inner(r) {
                    let (s12, res) = produce_comparison_rec(&ct1, t2, env, ctx1, ctx2, false);
                    if res > ComparisonResult::Different {
                        return (s12, res);
                    }
                }
            }
            _ => {
                if let Ok(cl) = l.normalize_in_ctx(env, ctx1) {
                    if let TermInner::Lambda(_ll, lr) = &cl.inner {
                        if let Ok(ct1) = lr.replace_inner(r) {
                            let (s12, res) =
                                produce_comparison_rec(&ct1, t2, env, ctx1, ctx2, false);
                            if res > ComparisonResult::Different {
                                return (s12, res);
                            }
                        }
                    }
                }
            }
        },
        _ => {}
    }
    match &t2.inner {
        TermInner::Global(g) => {
            if let Ok(ct2) = env.delta_replacement(*g) {
                let (s12, res) = produce_comparison_rec(t1, &ct2, env, ctx1, ctx2, false);
                if res > ComparisonResult::Different {
                    return (s12, res);
                }
            }
        }
        TermInner::Variable(v) => {
            if let Ok(ct2) = ctx2.delta_replacement(*v) {
                let (s12, res) = produce_comparison_rec(t1, &ct2, env, ctx1, ctx2, false);
                if res > ComparisonResult::Different {
                    return (s12, res);
                }
            }
        }
        TermInner::Apply(l, r) => match &l.inner {
            TermInner::Lambda(_ll, lr) => {
                if let Ok(ct2) = lr.replace_inner(r) {
                    let (s12, res) = produce_comparison_rec(t1, &ct2, env, ctx1, ctx2, false);
                    if res > ComparisonResult::Different {
                        return (s12, res);
                    }
                }
            }
            _ => {
                if let Ok(cl) = l.normalize_in_ctx(env, ctx2) {
                    if let TermInner::Lambda(_ll, lr) = &cl.inner {
                        if let Ok(ct2) = lr.replace_inner(r) {
                            let (s12, res) =
                                produce_comparison_rec(t1, &ct2, env, ctx1, ctx2, false);
                            if res > ComparisonResult::Different {
                                return (s12, res);
                            }
                        }
                    }
                }
            }
        },
        _ => {}
    }
    (
        adjust_strings(
            format!("`{}`", produce_term(t1)),
            format!("`{}`", produce_term(t2)),
        ),
        ComparisonResult::Different,
    )
}

fn produce_comparison(
    t1: &Term<TermMeta>,
    t2: &Term<TermMeta>,
    env: &Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
) -> (String, String) {
    let (r, _) = produce_comparison_rec(t1, t2, env, &mut ctx.clone(), ctx, false);
    r
}

fn generate_labels(filename: &str, v: &[(&TermMeta, String)]) -> Vec<(Range<usize>, String)> {
    let mut r = vec![];
    for p in v {
        let meta = p.0;
        let s = &p.1;
        if meta.filename == filename {
            r.push((meta.range.clone(), s.clone()));
        }
    }
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
                offset: found.meta.range.start,
                message: "mismatched type in definition".to_string(),
                note: {
                    let (exp, fnd) = produce_comparison(&expected, &found, &env, &mut ctx);
                    Some(format!(
                        "mismatched types\nexpected {}\n\nfound    {}",
                        exp, fnd
                    ))
                },
                help: None,
                labels: generate_labels(
                    filename,
                    &[
                        (&expected.meta, "type constrained by this".to_string()),
                        (&found.meta, "has unexpected type".to_string()),
                    ],
                ),
            });
        }
        Error::AppMismatchedType {
            lhs,
            expected,
            rhs,
            found,
            env,
            mut ctx,
        } => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: lhs.meta.range.start,
                message: "mismatched type in parameter".to_string(),
                note: {
                    let (exp, fnd) = produce_comparison(&expected, &found, &env, &mut ctx);
                    Some(format!(
                        "mismatched types\nexpected {}\n\nfound    {}",
                        exp, fnd
                    ))
                },
                help: None,
                labels: generate_labels(
                    filename,
                    &[
                        (&expected.meta, "type constrained by this".to_string()),
                        (&rhs.meta, "has unexpected type".to_string()),
                        (&lhs.meta, "function here".to_string()),
                    ],
                ),
            });
        }
        Error::AppInvalid { lhs, ltype, rhs } => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: lhs.meta.range.start,
                message: "applied expression is not a function".to_string(),
                note: Some(format!("its type is `{}`", produce_term(&ltype))),
                help: None,
                labels: generate_labels(
                    filename,
                    &[
                        (&lhs.meta, "not a function".to_string()),
                        (&rhs.meta, "applied to this parameter".to_string()),
                    ],
                ),
            });
        }
        Error::NonSort { expr, offending } => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: expr.range.start,
                message: "type of expression must be `Type` or some `Type`N".to_string(),
                note: Some(format!("its type is `{}`", produce_term(&offending))),
                help: None,
                labels: generate_labels(filename, &[(&expr, "has incorrect type".to_string())]),
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
