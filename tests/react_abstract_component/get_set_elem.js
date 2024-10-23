//@flow

declare var x: component(ref: React.RefSetter<any>, ...any);
declare var y: string;
x[y]; // error
x[y] = y; // error: no indexed type
