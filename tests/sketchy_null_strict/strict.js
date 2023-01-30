// @flow strict

import type { NullableType } from "./type";

function test(sketch: NullableType): void {
  if (sketch.nullableBool); // error because the file is strict
  if (sketch.nullableNum); // error because the file is strict
  if (sketch.nullableStr); // error because the file is strict
}
