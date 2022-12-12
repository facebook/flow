declare function f(x: number): boolean %checks (x === 0);
function f(x: mixed): boolean { return true }

f("a"); //error
declare var x: number;
if (f(x)) {
  (x: 0);
}

// inconsistent predicates; first predicate wins
declare function g(x: number): boolean %checks (x === 0);
function g(x: number): boolean %checks { return x === 1 }

if (g(x)) {
  (x: 0);
  (x: 1); // error
}

// declared function must be before regular function
function h(x: number): boolean %checks { return x === 1 }
declare function h(x: number): boolean %checks (x === 0);


//multiple declarations where at least one is a predicate is not allowed
declare function i(x: number): boolean %checks (x === 0);
declare function i(x: string): boolean %checks (x === "A");
function i(x: number): boolean %checks { return x === 1 }

//multiple declarations where at least one is a predicate is not allowed
declare function j(x: number): boolean;
declare function j(x: string): boolean %checks (x === "A");

declare function k(x: string): boolean %checks (x === "A");
declare function k(x: number): boolean;

declare function l(x: boolean): boolean;
declare function l(x: string): boolean %checks (x === "A");
declare function l(x: number): boolean;
