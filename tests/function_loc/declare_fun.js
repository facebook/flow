// @flow
declare function f(): void;
(f: empty); // err
module.exports = f;
