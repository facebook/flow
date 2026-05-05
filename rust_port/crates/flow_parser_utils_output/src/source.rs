/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Default)]
pub struct Source {
    buffer: String,
}

impl Source {
    pub fn create() -> Self {
        Self {
            buffer: String::with_capacity(127),
        }
    }

    pub fn add_string(&mut self, s: &str) -> &mut Self {
        self.buffer.push_str(s);
        self
    }

    pub fn add_identifier(&mut self, s: &str) -> &mut Self {
        self.add_string(s)
    }

    // TODO: Remove any right trailing whitespace
    pub fn add_newline(&mut self) -> &mut Self {
        self.buffer.push('\n');
        self
    }

    pub fn add_space(&mut self, n: usize) -> &mut Self {
        let spaces = " ".repeat(n);
        self.add_string(&spaces)
    }

    pub fn contents(self) -> String {
        self.buffer
    }
}
