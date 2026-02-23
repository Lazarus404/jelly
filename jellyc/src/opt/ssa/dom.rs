use std::collections::HashSet;

pub(super) fn compute_dom(preds: &[Vec<usize>], entry: usize) -> Vec<Vec<bool>> {
    let n = preds.len();
    let mut dom: Vec<Vec<bool>> = vec![vec![true; n]; n];
    for i in 0..n {
        if i == entry {
            dom[i] = vec![false; n];
            dom[i][entry] = true;
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        for b in 0..n {
            if b == entry {
                continue;
            }
            let mut new = vec![true; n];
            if preds[b].is_empty() {
                // Treat "no preds" blocks as separate roots; should be unreachable after cfg_simplify.
                new = vec![false; n];
            } else {
                for &p in &preds[b] {
                    for i in 0..n {
                        new[i] &= dom[p][i];
                    }
                }
            }
            new[b] = true;
            if new != dom[b] {
                dom[b] = new;
                changed = true;
            }
        }
    }
    dom
}

pub(super) fn compute_idom(dom: &[Vec<bool>], entry: usize) -> Vec<Option<usize>> {
    let n = dom.len();
    let mut idom: Vec<Option<usize>> = vec![None; n];
    idom[entry] = Some(entry);
    for b in 0..n {
        if b == entry {
            continue;
        }
        let strict: Vec<usize> = (0..n).filter(|&d| d != b && dom[b][d]).collect();
        if strict.is_empty() {
            idom[b] = Some(entry);
            continue;
        }
        let mut best = strict[0];
        let mut best_sz = dom[best].iter().filter(|x| **x).count();
        for &d in &strict[1..] {
            let sz = dom[d].iter().filter(|x| **x).count();
            if sz > best_sz {
                best = d;
                best_sz = sz;
            }
        }
        idom[b] = Some(best);
    }
    idom
}

pub(super) fn dom_tree_children(idom: &[Option<usize>], entry: usize) -> Vec<Vec<usize>> {
    let nb = idom.len();
    let mut children: Vec<Vec<usize>> = vec![Vec::new(); nb];
    for b in 0..nb {
        if b == entry {
            continue;
        }
        if let Some(p) = idom[b] {
            if p != b {
                children[p].push(b);
            }
        }
    }
    children
}

pub(super) fn dominance_frontiers(
    entry: usize,
    succs: &[Vec<usize>],
    children: &[Vec<usize>],
    idom: &[Option<usize>],
) -> Vec<HashSet<usize>> {
    let nb = succs.len();
    let mut df: Vec<HashSet<usize>> = vec![HashSet::new(); nb];

    for b in 0..nb {
        for &s in &succs[b] {
            if idom[s] != Some(b) {
                df[b].insert(s);
            }
        }
    }

    // Post-order over dom tree.
    fn post(b: usize, children: &[Vec<usize>], out: &mut Vec<usize>) {
        for &c in &children[b] {
            post(c, children, out);
        }
        out.push(b);
    }
    let mut order = Vec::new();
    post(entry, children, &mut order);

    for &b in &order {
        for &c in &children[b] {
            let from_child: Vec<usize> = df[c].iter().copied().collect();
            for w in from_child {
                if idom[w] != Some(b) {
                    df[b].insert(w);
                }
            }
        }
    }

    df
}
