
/* @providesModule Arrays */

function foo(x:string) { }

var a = [];
a[0] = 1;
a[1] = "...";

foo(a[1]);
var y;
a.forEach(x => y=x);


module.exports = "arrays";
