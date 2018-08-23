// @flow

import type {NullableType} from './flowStrictTypeFile';

function printSketchyBoolean(sketch: NullableType): void {
  if (sketch.nullableBool) {
    console.log(sketch.nullableBool);
  }
  if (sketch.nullableNum) {
    console.log(sketch.nullableNum);
  }
  if (sketch.nullableStr) {
    console.log(sketch.nullableStr);
  }
}
