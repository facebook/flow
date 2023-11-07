//@flow

function f() {
  x.push('a');
}

var x = [];

x as Array<number>;

var y: Array<number> = [];
[] as Array<number>;
