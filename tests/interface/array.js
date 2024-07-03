declare var a: Array<number>;

(a: interface {pop(): number | void}); // OK
(a: interface {pop(): boolean | void}); // ERROR
