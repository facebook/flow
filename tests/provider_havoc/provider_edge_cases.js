//@flow

var x = 42;
declare export default class C { }
if (((x: mixed) => 42) === console.log) { // constant-condition error
}

f = function() {
  f = 42;
};
