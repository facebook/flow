declare var a: Array<number>;

a as interface {pop(): number | void}; // OK
a as interface {pop(): boolean | void}; // ERROR
