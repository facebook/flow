// Repeated inline exports merge generic interface members.
export interface Tippy<T = number> { a: T } // OK
export interface Tippy<T = number> { b: string } // OK
export interface Tippy<T = number> { c: boolean } // OK

// Interfaces also merge when exported once through an export specifier.
interface Control { a: number } // OK
interface Control { b: string } // OK
export { Control }; // OK
