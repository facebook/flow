var a = [true,false];
function foo(x: mixed) { }

for (var i=0;i<3;i++) {
    foo(a[i]);
}
for (const k in a) {
    foo(a[k]); // k is a string, which shouldn't be used for array access
}

var b = (null : ?{[key: string]: string});
for (const j in b) {
    foo(b[j]);
}

var c;
for (const m in (c = b)) {
    foo(c[m]);
}

var d;
for (const n in (d = a)) {
    foo(d[n]); // d is a string, which shouldn't be used for array access
}

for (const x1 in undefined) {
    foo(x1); // unreachable
}

for (const x2 in null) {
    foo(x2); // unreachable
}

for (const y in this) {
    // regression test to make sure `in this` doesn't fatal. it's currently
    // allowed, even though we can't actually enumerate all the keys on `this`.
}
