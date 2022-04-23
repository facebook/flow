//@flow

function g() {
    return f();
}

function f() {
    return 42;
}

var x = (function h() { (h(): empty); return 42 }); // err

(f(): empty); // err
(g(): empty); // err


function h(): number {
    return i();
}

function i(): number {
    return h();
}

var y = (function k(): number { (k(): empty); return 42 }); // err

(h(): empty); // err
(i(): empty); // err
