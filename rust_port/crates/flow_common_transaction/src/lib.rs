/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub struct Mutator<'a> {
    commit: Box<dyn FnOnce() + 'a>,
    rollback: Box<dyn FnOnce() + 'a>,
}

/// `'a` bounds what the mutators may borrow. A transaction never escapes the scope that runs it —
/// `with_transaction_sync` and `with_transaction_result_sync` commit or roll it back before
/// returning — so a mutator can borrow from that scope and does not have to own everything it
/// touches.
pub struct Transaction<'a> {
    name: String,
    mutators: Vec<Mutator<'a>>,
    committed: bool,
}

pub fn add<'a>(
    transaction: &mut Transaction<'a>,
    commit: impl FnOnce() + 'a,
    rollback: impl FnOnce() + 'a,
) {
    transaction.mutators.push(Mutator {
        commit: Box::new(commit),
        rollback: Box::new(rollback),
    });
}

pub fn commit(transaction: &mut Transaction<'_>) {
    flow_hh_logger::info!("Committing transaction: {}", transaction.name);
    for mutator in std::mem::take(&mut transaction.mutators).into_iter().rev() {
        (mutator.commit)();
    }
    transaction.committed = true;
}

pub fn rollback(transaction: &mut Transaction<'_>) {
    flow_hh_logger::info!("Rolling back transaction: {}", transaction.name);
    for mutator in std::mem::take(&mut transaction.mutators).into_iter().rev() {
        (mutator.rollback)();
    }
}

impl Drop for Transaction<'_> {
    fn drop(&mut self) {
        if !self.committed && !self.mutators.is_empty() {
            rollback(self);
        }
    }
}

pub fn with_transaction_sync<'a, T>(name: &str, f: impl FnOnce(&mut Transaction<'a>) -> T) -> T {
    let mut transaction = Transaction {
        name: name.to_string(),
        mutators: Vec::new(),
        committed: false,
    };
    let result = f(&mut transaction);
    commit(&mut transaction);
    result
}

pub fn with_transaction_result_sync<'a, T, E>(
    name: &str,
    f: impl FnOnce(&mut Transaction<'a>) -> Result<T, E>,
) -> Result<T, E> {
    let mut transaction = Transaction {
        name: name.to_string(),
        mutators: Vec::new(),
        committed: false,
    };
    match f(&mut transaction) {
        Ok(result) => {
            commit(&mut transaction);
            Ok(result)
        }
        Err(err) => Err(err),
    }
}
