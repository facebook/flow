var x;

component C() {
    x = 42;
    return 42 as any
}

x = 'a';

function havoced() {
    x as empty;
}
