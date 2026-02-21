type T = string;
type S = number;
type R = boolean;
type X = string | number;
const value: string = "hello";

// Export type specifier on a regular export
export {type T}; // OK

// Export type specifier with source
export {type S} from './exports.js'; // OK

// Mixed value and type export specifiers
export {value, type R}; // OK

// Renamed type export
export {type T as RenamedT}; // OK

// Error: export type { type X }
export type {type X}; // ERROR
