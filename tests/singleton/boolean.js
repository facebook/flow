/* @flow */

function veryOptimistic(isThisAwesome: true): boolean {
  return isThisAwesome;
}

var x1 : boolean = veryOptimistic(true);
var y1 : boolean = veryOptimistic(false); // error

function veryPessimistic(isThisAwesome: true): boolean {
  return !isThisAwesome; // test boolean conversion
}

var x2 : boolean = veryPessimistic(true);
var y2 : boolean = veryPessimistic(false); // error

type MyOwnBooleanLOL = true | false

function bar(x: MyOwnBooleanLOL): false {
  if (x) {
    return x;
  } else {
    return !x;
  }
}

bar(true);
bar(false);
bar(1); // error

function alwaysFalsy(x: boolean): false {
  if (x) {
    return !x;
  } else {
    return x;
  }
}

function triv () : true {
  declare var x : false | false;
  return !x;
}

function triv2 () : false {
  declare var x : true | {};
  return !x;
}
