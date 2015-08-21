declare var a:string;
declare var b:string;
declare var c:string;
[{a1:a, b},c] = [{a1:0, b:1},2];

var {m} = {m:0};
({m} = {m:m});

var obj;
var {n: obj.x};
var [obj.x];

function foo({p, z:[r]}) {
    a = p;
    b = z;
    c = r;
}
foo({p:0, z:[1,2]});

[a,,b,...c] = [0,1,true,3];

function bar({x, ...z}) {
    var o:{x: string; y: number;} = z;
}
bar({x:"",y:0});

var spread = {y:""};
var extend: {x:number; y:string; z: boolean} = {x:0, ...spread};

function qux(_: {a:number}) { }
qux({a:""});
function corge({b}: {b:string}) { }
corge({b:0});

var {n}:{n: number} = {n: ""}

function test() {
  var {foo} = {bar: 123}; // error on foo
  var {bar, baz} = {bar: 123} // error on baz
}

function test() {
  var x = {foo: 'abc', bar: 123};
  var {foo, ...rest} = x;
  (x.baz: string); // error, baz doesn't exist
  (rest.baz: string); // no error, rest is unsealed
}

module.exports = corge;
