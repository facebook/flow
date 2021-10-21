// @flow

// TODO: In case of infered type size violation, this should be annotated as `any`
function foo(x) {}

foo({f: 1, g: 2, h: 3});
