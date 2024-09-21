declare class Foo extends React$Component<{bar: string}> {}
declare class ImNotARefSetter {}

Foo as component(bar: string, ref: React.RefSetter<Foo>); // ok
Foo as component(bar: number, ref: React.RefSetter<Foo>); // error: number ~> string
Foo as component(ref: React.RefSetter<Foo>, ...{bar: number}); // error: number ~> string
Foo as component( // error: Foo ~> string
    bar: string,
    ref: React.RefSetter<string>
);
Foo as component(
    bar: string,
    ref: ImNotARefSetter // error: bad ref
);
