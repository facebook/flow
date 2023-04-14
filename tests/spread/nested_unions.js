declare var OBJ: {a: string};

declare var works1: {...} | void;
declare var works2: {...} | void | void;
declare var works3: {...} | (void | void);

({...OBJ, ...works1}); // error: cannot-spread-inexact
({...OBJ, ...works2}); // error: cannot-spread-inexact
({...OBJ, ...works3}); // error: cannot-spread-inexact

type A   = { t: 'a' };
type BC  = { t: 'b' } | { t: 'c' };
type ABC = { t: 'a' } | { t: 'b' } | { t: 'c' };

declare var x1: ABC;
declare var x2: A | BC;
const o1: BC = { ...x1 }; // errors
const o2: BC = { ...x2 }; // errors
