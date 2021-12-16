// @flow

// Should be annotated with `NumType`, not `NumType | number`
function g(x) {}
type NumType = number;
const y: NumType = 1;
g(y);
g(2);

// Should be annotated with `NumType | StringType`, not `NumType | StringType | string`
function g2(x) {}
type StringType = string;
const z: NumType | StringType = 1;
g2(z);
g2('test');

// Should be annotated with `?StringType`
function g3(x) {}
declare var w: ?StringType;
g3(w);
g3('test');
