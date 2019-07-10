// @flow strict

import type { NullableBigInt } from './flowTypeFile';

function printSketchyNumber(sketch: NullableBigInt): void {
  if (sketch.nullableBigInt) {
    console.log(sketch.nullableBigInt);
  }
}
