//@flow

declare const x: component(ref: React.RefSetter<any>, ...any);
declare const y: string;
x[y]; // error
x[y] = y; // error: no indexed type
