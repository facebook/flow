// `class`, `declare class`, and `enum` populate the value namespace and (per
// TypeScript) also the type namespace. They cannot coexist with a `type` alias
// of the same name. The class/declare-class + interface combinations are
// permitted as a carve-out for declaration-merging support (deferred).

class C1 {}
type C1 = number; // ERROR

type C2 = number; // ERROR
class C2 {}

declare class D1 {}
type D1 = number; // ERROR

type D2 = number; // ERROR
declare class D2 {}

enum E1 { X = 'x' }
type E1 = number; // ERROR

enum E2 { X = 'x' }
interface E2 {} // ERROR (enum + interface remains an error)
