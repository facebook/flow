/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Mutex;

static REPORT_CANCELED_CALLBACK: Mutex<Option<Box<dyn Fn(i32, i32) + Send>>> = Mutex::new(None);

pub fn set_report_canceled_callback(callback: impl Fn(i32, i32) + Send + 'static) {
    *REPORT_CANCELED_CALLBACK.lock().unwrap() = Some(Box::new(callback));
}

pub fn report_canceled(total: i32, finished: i32) {
    if let Some(ref callback) = *REPORT_CANCELED_CALLBACK.lock().unwrap() {
        callback(total, finished);
    }
}
