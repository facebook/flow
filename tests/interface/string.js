declare const s: string;

// Both are actually OK at runtime, but we ban it in Flow
(s: interface {charAt(pos: number): string}); // ERROR
for (const c of s) {} // ERROR

// Wrapping in `new String(...)` allows this usage
(new String(s): interface {charAt(pos: number): string}); // OK
for (const c of new String(s)) {} // OK
