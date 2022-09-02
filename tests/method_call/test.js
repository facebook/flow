//@flow

function foo(this: {|foo: () => number, x: number|}) {
    return this.x;
}
let o1 = { foo, x : 3 }

function bar(this: {|foo: () => number, y: number|}) {
    return this.y;
}
let o2 = { foo : bar, y : 3}

let x = true ? o1 : o2;

x.foo();
