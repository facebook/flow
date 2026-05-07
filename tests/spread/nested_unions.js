declare const OBJ: {a: string, ...};

declare const works1: {...} | void;
declare const works2: {...} | void | void;
declare const works3: {...} | (void | void);

({...OBJ, ...works1}); // error: cannot-spread-inexact
({...OBJ, ...works2}); // error: cannot-spread-inexact
({...OBJ, ...works3}); // error: cannot-spread-inexact

type A   = { t: 'a', ... };
type BC  = { t: 'b', ... } | { t: 'c', ... };
type ABC = { t: 'a', ... } | { t: 'b', ... } | { t: 'c', ... };

declare const x1: ABC;
declare const x2: A | BC;
const o1: BC = { ...x1 }; // errors
const o2: BC = { ...x2 }; // errors
