// @flow strict

import type {NullableString} from './flowTypeFile';

function printSketchyString(sketch: NullableString): void {
  if (sketch.nullableStr) {
    console.log(sketch.nullableStr);
  }
}
