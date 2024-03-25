type Foo<T> = { foo: T };
type Bar<T> = { bar: T };

type Bak<T = mixed> = Foo<Bar<T>>;

function test1() {
    declare const x: Bak<>;
    x as Bak<empty>; // error mixed <~> empty
}

function test2() {
    declare var x: Bak<boolean>;
    x as Bak<number> | Bak<string>; // error Bak<boolean> ~/~> Bak<number> | Bak<string>
}
