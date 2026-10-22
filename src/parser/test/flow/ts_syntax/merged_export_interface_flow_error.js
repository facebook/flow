// Repeated interface exports remain errors in Flow files.
export interface Tippy { a: number }
export interface Tippy { b: string } // ERROR
