type Bar = [{baz: Array<string>}];
type Foo = {bar: Bar};

declare function ouchWithNamed(foo: Foo): void;
ouchWithNamed({bar: [{baz: [3]}]});

type Foo2 = {bar: Bar, bus: Bar};
declare function ouchWithNamed2(foo: Foo2): void;
ouchWithNamed2({bar: [{baz: [3]}], bus: [{baz: [3]}]});

type Foo3 = {bar: Bar2};
type Bar2 = {baz: Baz};
type Baz = {ouch: string};
declare function ouchWithNamed3(foo: Foo3): void;
ouchWithNamed3({bar: {baz: {ouch: 3}}});

declare function ouchUnnamed(foo: {bar: [{baz: Array<string>}]}): void;
ouchUnnamed({bar: [{baz: [3]}]});
