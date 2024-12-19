// @flow

type bar = string
const baz : bar = "baz"

// should only suggest values, not types
  b
// ^
