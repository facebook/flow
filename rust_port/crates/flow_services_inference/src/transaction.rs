/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Mutator {
    commit: Box<dyn FnOnce()>,
    rollback: Box<dyn FnOnce()>,
}

pub struct Transaction {
    name: String,
    mutators: Vec<Mutator>,
    committed: bool,
}

impl Transaction {
    // (* let add ~commit ~rollback transaction = transaction := { commit; rollback } :: !transaction *)
    pub fn add(&mut self, commit: impl FnOnce() + 'static, rollback: impl FnOnce() + 'static) {
        self.mutators.push(Mutator {
            commit: Box::new(commit),
            rollback: Box::new(rollback),
        });
    }
}

fn commit(transaction: &mut Transaction) {
    eprintln!("Committing transaction: {}", transaction.name);
    for mutator in std::mem::take(&mut transaction.mutators).into_iter().rev() {
        (mutator.commit)();
    }
    transaction.committed = true;
}

fn rollback(transaction: &mut Transaction) {
    eprintln!("Rolling back transaction: {}", transaction.name);
    for mutator in std::mem::take(&mut transaction.mutators).into_iter().rev() {
        (mutator.rollback)();
    }
}

impl Drop for Transaction {
    fn drop(&mut self) {
        if !self.committed && !self.mutators.is_empty() {
            rollback(self);
        }
    }
}

pub fn with_transaction_sync<T>(name: &str, f: impl FnOnce(&mut Transaction) -> T) -> T {
    let mut transaction = Transaction {
        name: name.to_string(),
        mutators: Vec::new(),
        committed: false,
    };
    let result = f(&mut transaction);
    commit(&mut transaction);
    result
}
