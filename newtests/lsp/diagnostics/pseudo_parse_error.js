// @thisWillBeFlowInTest

const obj = {};
// Flow does not yet support method or property calls in optional chains, so
// this will produce a pseudo parse error
obj?.foo(); // Error
