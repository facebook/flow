// @flow

// This error exists in c.js but will only be found if c.js is type-checked.
// c.js is a transitive dependency of a.js (via b.js), so it gets merged
// but not type-checked when only a.js is focused.
const x: string = 42;

export default x;
