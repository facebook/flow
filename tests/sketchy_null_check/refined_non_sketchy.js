declare var x: ?boolean;
declare var o: {p: ?boolean};

function f1() {
    if (x == null) return;
    if (x) {}
}

function f2() {
    if (o.p == null) return;
    if (o.p) {}
}
