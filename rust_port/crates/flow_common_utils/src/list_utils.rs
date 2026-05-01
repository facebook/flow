/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashSet;
use std::hash::Hash;
use std::rc::Rc;

use dupe::Dupe;
use flow_data_structure_wrapper::list::FlowOcamlList;

// this function takes a list and truncates it if needed to no more than
/// the first n elements. If truncation happened, then the callback 'f'
/// is used to generated a final element e.g. "shown 5/200"
pub fn first_upto_n<T, F>(n: usize, f: F, lst: Vec<T>) -> Vec<T>
where
    F: FnOnce(usize) -> Option<T>,
{
    let mut first: Vec<T> = Vec::new();
    let mut total: usize = 0;
    for s in lst {
        if total < n {
            first.push(s);
        }
        total += 1;
    }
    if total <= n {
        first
    } else {
        match f(total) {
            None => first,
            Some(e) => {
                first.push(e);
                first
            }
        }
    }
}

/// performs a map, but returns the original list if there is no change
pub fn ident_map<T, F, Same>(mut f: F, mut same: Same, lst: Rc<[T]>) -> Rc<[T]>
where
    T: Dupe,
    F: FnMut(&T) -> T,
    Same: FnMut(&T, &T) -> bool,
{
    let mut result: Option<Vec<T>> = None;
    for (i, item) in lst.iter().enumerate() {
        let item_prime = f(item);
        if let Some(vec) = result.as_mut() {
            vec.push(item_prime);
        } else if !same(&item_prime, item) {
            let mut vec = Vec::with_capacity(lst.len());
            vec.extend(lst[..i].iter().map(|t| t.dupe()));
            vec.push(item_prime);
            result = Some(vec);
        }
    }
    match result {
        Some(vec) => vec.into(),
        None => lst,
    }
}

pub fn try_ident_map<T, F, Same, E>(mut f: F, mut same: Same, lst: Rc<[T]>) -> Result<Rc<[T]>, E>
where
    T: Dupe,
    F: FnMut(&T) -> Result<T, E>,
    Same: FnMut(&T, &T) -> bool,
{
    let mut result: Option<Vec<T>> = None;
    for (i, item) in lst.iter().enumerate() {
        let item_prime = f(item)?;
        if let Some(vec) = result.as_mut() {
            vec.push(item_prime);
        } else if !same(&item_prime, item) {
            let mut vec = Vec::with_capacity(lst.len());
            vec.extend(lst[..i].iter().map(|t| t.dupe()));
            vec.push(item_prime);
            result = Some(vec);
        }
    }
    Ok(match result {
        Some(vec) => vec.into(),
        None => lst,
    })
}

pub fn ident_map_ocaml_list<T, F, Same>(
    mut f: F,
    mut same: Same,
    lst: FlowOcamlList<T>,
) -> (FlowOcamlList<T>, bool)
where
    T: Clone + Dupe,
    F: FnMut(&T) -> T,
    Same: FnMut(&T, &T) -> bool,
{
    let mut result: Option<Vec<T>> = None;
    for (i, item) in lst.iter().enumerate() {
        let item_prime = f(item);
        if let Some(vec) = result.as_mut() {
            vec.push(item_prime);
        } else if !same(&item_prime, item) {
            let mut vec = Vec::with_capacity(lst.len());
            vec.extend(lst.iter().take(i).map(|t| t.dupe()));
            vec.push(item_prime);
            result = Some(vec);
        }
    }
    match result {
        Some(vec) => (vec.into_iter().collect(), true),
        None => (lst, false),
    }
}

pub fn zipi<X, Y>(xs: Vec<X>, ys: Vec<Y>) -> Vec<(usize, X, Y)> {
    assert_eq!(
        xs.len(),
        ys.len(),
        "ListUtils.zipi: lists of unequal length"
    );
    xs.into_iter()
        .zip(ys)
        .enumerate()
        .map(|(i, (x, y))| (i, x, y))
        .collect()
}

// Stringify a list given a separator and a printer for the element type
pub fn to_string<T, F>(separator: &str, printer: F, list: &[T]) -> String
where
    F: Fn(&T) -> String,
{
    list.iter().map(printer).collect::<Vec<_>>().join(separator)
}

// Dedups a list in O(n) time and space. Unlike Base.List.dedup, this
// preserves order. Core's implementation is also O(n log n)
pub fn dedup<T>(l: Vec<T>) -> Vec<T>
where
    T: Eq + Hash + Clone,
{
    let mut tbl: HashSet<T> = HashSet::with_capacity(l.len());
    let mut result: Vec<T> = Vec::new();
    for e in l.into_iter() {
        if !tbl.contains(&e) {
            tbl.insert(e.clone());
            result.push(e);
        }
    }
    result
}
