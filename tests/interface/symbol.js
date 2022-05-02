declare var s: symbol;

(s: interface {+description: string | void}); // OK
(s: interface {+description: boolean}); // ERROR
