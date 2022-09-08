// @flow

function a() {}
function a() {} // error: illegal binding

const b = () => {}
const b = () => {} // error: illegal binding

const c = function () {}
const c = function () {} // error: illegal binding
