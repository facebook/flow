/* @flow */

class X1 { foo: number; };
class X2 { foo: string; };

function x(b) { return b ? new X1 : new X2; }

function consumer(b) {
    var g = x(b);
    if (g instanceof X2) g.foo = '1337';
    else g.foo = 1337;
}
