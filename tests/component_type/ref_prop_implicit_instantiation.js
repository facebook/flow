declare const C: component(ref: React.RefSetter<Promise<string>>);

declare function extractInstance<I>(c: component(ref: React.RefSetter<I>)): I;
const instance = extractInstance(C);
instance as Promise<string>; // error, because instance is inferred as mixed above. This is bad.
instance as empty; // error

declare function extractRefSetter<S>(c: component(ref: S)): S; // error: bad ref
const refSetter = extractRefSetter(C); // underconstrained, Promise incompatible with empty???
refSetter as React.RefSetter<Promise<string>>; // no error, refSetter is any
refSetter as empty; // no error, refSetter is any
