// Type aliases cannot share an exported name with an interface in either order.
export interface AliasCollision {}
export type AliasCollision = number; // ERROR
export type ReverseCollision = number;
export interface ReverseCollision {} // ERROR

// Explicit type exports still conflict with merged inline interface exports.
export interface ExplicitExport { a: number }
export interface ExplicitExport { b: string }
export type { ExplicitExport }; // ERROR

// Repeated value exports remain errors.
declare const value: number;
export { value };
export { value }; // ERROR
