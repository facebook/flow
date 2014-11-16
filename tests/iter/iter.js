var a = [true,false];
function foo(x) { }

for (var i=0;i<3;i++) {
    foo(a[i]);
}
for (var k in a) {
    foo(a[k]);
}
