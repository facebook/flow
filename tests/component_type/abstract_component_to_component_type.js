declare const Foo: React$AbstractComponent<{+bar: string}, Instance>;
declare class Instance {}
declare class ImNotARefSetter {}

Foo as component(bar: string, ref: React.RefSetter<Instance>); // ok
Foo as component(bar: number, ref: React.RefSetter<Instance>); // error: number ~> string
Foo as component(ref: React.RefSetter<Instance>, ...{bar: number}); // error: number ~> string
Foo as component( // error: Instance ~> string
    bar: string,
    ref: React.RefSetter<string>
);
Foo as component( // error again due to bad ref
    bar: string,
    ref: ImNotARefSetter // error: bad ref
);
