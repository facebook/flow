/* @flow */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({addFiles, addCode, flowCmd}) => [
  test('should trigger sketchy null output on sketchy string', [
    addFiles('flowTypeFile.js', 'flowStrictSketchyStringFile.js')
      .newErrors(
        `
          flowStrictSketchyStringFile.js:6
            6:   if (sketch.nullableStr) {
                     ^^^^^^^^^^^^^^^^^^ Sketchy null check on string [1] which is potentially an empty string. Perhaps you meant to check for null or undefined [1]? [sketchy-null-string]
            References:
              4:   nullableStr?: string,
                                 ^^^^^^ [1]. See: flowTypeFile.js:4
        `,
      ).exitCodes([]),
  ]),
  test('should trigger sketchy null output on sketchy number', [
    addFiles('flowTypeFile.js', 'flowStrictSketchyNumberFile.js')
    .newErrors(
      `
        flowStrictSketchyNumberFile.js:6
          6:   if (sketch.nullableNum) {
                   ^^^^^^^^^^^^^^^^^^ Sketchy null check on number [1] which is potentially 0. Perhaps you meant to check for null or undefined [1]? [sketchy-null-number]
          References:
            8:   nullableNum?: number,
                               ^^^^^^ [1]. See: flowTypeFile.js:8
      `,
    ).exitCodes([]),
  ]),
  test('should trigger sketchy null output on sketchy bool', [
    addFiles('flowTypeFile.js', 'flowStrictSketchyBooleanFile.js')
    .newErrors(
      `
        flowStrictSketchyBooleanFile.js:6
          6:   if (sketch.nullableBool) {
                   ^^^^^^^^^^^^^^^^^^^ Sketchy null check on boolean [1] which is potentially false. Perhaps you meant to check for null or undefined [1]? [sketchy-null-bool]
          References:
           12:   nullableBool?: boolean,
                                ^^^^^^^ [1]. See: flowTypeFile.js:12
      `,
    ).exitCodes([]),
  ]),
  test('should not trigger sketchy null output on non-strict file', [
    addFiles('flowStrictTypeFile.js', 'flowSketchyFile.js')
    .noNewErrors(),
  ]),
]);
