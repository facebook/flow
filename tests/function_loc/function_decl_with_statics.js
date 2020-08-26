// @flow
function f() {};
f.x = 1;
(f: empty); // err
(f.x: empty); // err
module.exports = f;
