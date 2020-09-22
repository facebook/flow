// @flow

class B {}
const A = {B};
module.exports = (new B: A.B);
