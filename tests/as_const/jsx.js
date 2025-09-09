const a = <div /> as const; // stay as ExactReactElement_DEPRECATED<'div'>
const b = <div />; // generalized to React.MixedElement

a as ExactReactElement_DEPRECATED<'div'>; // ok
b as ExactReactElement_DEPRECATED<'div'>; // error
a as empty; // error
b as empty; // error
