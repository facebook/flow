//@flow

function g() {
    return f();
}

function f() {
    return 42;
}

var x = (function h() { (h(): empty); return 42 });

(f(): empty);
(g(): empty);


function h(): number {
    return i();
}

function i(): number {
    return h();
}

var y = (function k(): number { (k(): empty); return 42 });

(h(): empty);
(i(): empty);
