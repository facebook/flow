// @flow

declare class Foo {
    foo(): number;
}

class Bar extends Foo {
    bar(s: string) {

    }
}

interface Qux {
    qux(): void;
}

interface I {
    i(x: number): string;
}

class Baz extends Bar implements Qux, I {
    
// ^
}

type O = {
    foo(): string;
}

const o: O = {
    
// ^
}
