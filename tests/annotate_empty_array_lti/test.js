// @flow

// $FlowFixMe[missing-empty-array-annot]
let z = []; // no annot
z = [4, 5];

const s = new Set([]);  // should annot
(s: Set<string>);
