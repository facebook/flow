/* @flow */
import {suite, test} from '../../../tsrc/test/Tester';

export default suite(({addFiles}) => [
  test('legacy test', [
    addFiles(
      'dupe1.js',
      'dupe2.js',
      'requires_dupe.js',
    ).newErrors(
`dupe2.js:0
Dupe. Duplicate module provider
current provider. See: dupe1.js:0`,
),
  ]),
]);
