type Bar = [{baz: Array<string>}];
type Foo = {bar: Bar};

declare function ouchWithNamed(foo: Foo): void;
ouchWithNamed({bar: [{baz: [3]}]});

const a: string | boolean = 3;
