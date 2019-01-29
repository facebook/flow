// @flow strict

import type {NullableNumber} from './flowTypeFile';

function printSketchyNumber(sketch: NullableNumber): void {
  if (sketch.nullableNum) {
    console.log(sketch.nullableNum);
  }
}
