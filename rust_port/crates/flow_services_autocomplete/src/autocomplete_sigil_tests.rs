/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/autocomplete/__tests__/autocomplete_sigil_tests.ml`

#[cfg(test)]
mod tests {
    use crate::autocomplete_sigil;

    #[test]
    fn empty_line() {
        let contents = "// @flow\n\nfoo\n\nbar";
        let expected = "// @flow\n\nfoo\nAUTO332\nbar";
        let (actual, broader_context, _) = autocomplete_sigil::add(None, contents, 4, 0);
        assert_eq!(expected, actual);
        let expected = "foo\nAUTO332\nbar";
        assert_eq!(expected, broader_context);
    }

    #[test]
    fn last_line() {
        let contents = "// @flow\n";
        let expected = "// @flow\nAUTO332";
        let (actual, broader_context, _) = autocomplete_sigil::add(None, contents, 2, 0);
        assert_eq!(expected, actual);
        let expected = "// @flow\nAUTO332";
        assert_eq!(expected, broader_context);
    }
}
