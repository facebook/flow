/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Performs a map, but returns the original optional if there is no change
pub fn ident_map<T, F, Same>(f: F, same: Same, x: Option<T>) -> Option<T>
where
    F: FnOnce(&T) -> T,
    Same: FnOnce(&T, &T) -> bool,
{
    match x {
        None => None,
        Some(x_prime) => {
            let x_double_prime = f(&x_prime);
            if same(&x_double_prime, &x_prime) {
                Some(x_prime)
            } else {
                Some(x_double_prime)
            }
        }
    }
}

pub fn try_ident_map<T, F, Same, E>(f: F, same: Same, x: Option<T>) -> Result<Option<T>, E>
where
    F: FnOnce(&T) -> Result<T, E>,
    Same: FnOnce(&T, &T) -> bool,
{
    match x {
        None => Ok(None),
        Some(x_prime) => {
            let x_double_prime = f(&x_prime)?;
            if same(&x_double_prime, &x_prime) {
                Ok(Some(x_prime))
            } else {
                Ok(Some(x_double_prime))
            }
        }
    }
}
