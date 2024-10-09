declare const C: component(ref: React.RefSetter<Promise<string>>);

declare function extractInstance<I>(c: component(ref: React.RefSetter<I>)): I;
const instance = extractInstance(C);
instance as Promise<string>; // ok
instance as empty; // error

declare function extractRefSetter<S>(c: component(ref: S)): S; // error: bad ref
const refSetter = extractRefSetter(C); // ok
refSetter as React.RefSetter<Promise<string>>; // ok
refSetter as empty; // error