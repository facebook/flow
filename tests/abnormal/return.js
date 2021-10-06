// @flow

function bar(x:number) { }
function foo() {
    var x: null = null;
    if (x == null) return;
    bar(x);
}
