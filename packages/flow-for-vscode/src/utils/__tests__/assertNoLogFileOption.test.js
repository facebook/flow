/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* eslint-disable playlyfe/indent-template-strings */

import assertNoLogFileOption from '../assertNoLogFileOption';

test('matches log.file', () => {
  const config = `
[options]
log.file=foo
  `;
  expect(() =>
    assertNoLogFileOption(config),
  ).toThrowErrorMatchingInlineSnapshot(
    `"Unsupported .flowconfig option \`log.file\`. The VS Code extension does not support this option."`,
  );
});

test('does not fail without log.file', () => {
  const config = `
[options]
  `;
  expect(assertNoLogFileOption(config)).toBeUndefined();
});
