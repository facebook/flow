declare var s: string;

(s: interface {charAt(pos: number): string}); // OK
(s: interface {charAt(pos: number): boolean}); // ERROR
