declare type Foo = React.AbstractComponent<{+bar: string}, Instance>;
declare const C1: component(bar: string, ref: React.RefSetter<Instance>);
declare const C2: component(ref: React.RefSetter<Instance>, ...{bar: string})
declare const C3: component(bar: string, ref: ImNotARefSetter); // error: bad ref
declare class Instance {}
declare class ImNotARefSetter {}

C1 as React.AbstractComponent<{+bar: string}>; // ok
C1 as React.AbstractComponent<{+bar: string}, Instance>; // ok
C2 as React.AbstractComponent<{+bar: string}>; // ok
C2 as React.AbstractComponent<{+bar: string}, Instance>; // ok
C1 as React.AbstractComponent<{+bar: number}, Instance>; // error: number ~> string
C2 as React.AbstractComponent<{+bar: number}, Instance>; // error: number ~> string
C1 as React.AbstractComponent<{+bar: string}, string>; // error: Instance ~> string
C3 as React.AbstractComponent<{+bar: string}, Instance>; // error: mixed ~> Instance
