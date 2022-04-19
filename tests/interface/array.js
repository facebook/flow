declare var a: Array<number>;

(a: interface {pop(): number}); // OK
(a: interface {pop(): boolean}); // ERROR
