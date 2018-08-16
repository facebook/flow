// @flow strict

import type {NullableBool} from './flowTypeFile';

function printSketchyBoolean(sketch: NullableBool): void {
  if (sketch.nullableBool) {
    console.log(sketch.nullableBool);
  }
}
