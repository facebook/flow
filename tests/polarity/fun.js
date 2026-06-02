type Arg<T> = T => void; // OK
type ArgNeg<in T> = T => void; // OK
type ArgPos<out T> = T => void; // Error: out T in negative position
type FlipArgNeg<in T> = (T => void) => void; // Error: in T in positive position
type FlipArgPos<out T> = (T => void) => void; // OK
type Ret<T> = () => T; // OK
type RetNeg<in T> = () => T; // Error: in T in positive position
type RetPos<out T> = () => T; // OK
type FlipRetNeg<in T> = (() => T) => void; // OK
type FlipRetPos<out T> = (() => T) => void; // Error: out T in negative position

function func_decl<out T>(): void {} // error
const func_expr = function <out T>() {} // error
const arrow_expr = <out T>() => {} // error
const obj = { arg<out T>(): void {} } // error
