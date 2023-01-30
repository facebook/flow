// @flow

import type { NullableType } from "./type";

function test(sketch: NullableType): void {
  if (sketch.nullableBool); // no error because because the file is non-strict
  if (sketch.nullableNum); // no error because because the file is non-strict
  if (sketch.nullableStr); // no error because because the file is non-strict
}
