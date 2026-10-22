// Repeated exported interfaces merge in TypeScript declaration files.
export interface Tippy<T = number> { a: T }
export interface Tippy<T = number> { b: string }
export interface Tippy<T = number> { c: boolean }
