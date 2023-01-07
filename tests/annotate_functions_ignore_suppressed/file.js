// @flow

const foo = function (x) {}; // adds annot
foo(1);

// $FlowFixMe
const bar = function (x) {}; // does not add annot
bar(1);

// $FlowFixMe[missing-local-annot]
const baz = function (x) {}; // does not add annot
baz(1);

// $FlowFixMe[prop-missing]
const bak = function (x) {}; // adds annot due to irrelevant code
bak(1);
