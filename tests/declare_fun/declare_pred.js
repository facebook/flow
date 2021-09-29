declare function f(x: number): bool %checks (x === 0);
function f(x: mixed): bool { return true }

f("a"); //error
declare var x: number;
if (f(x)) {
  (x: 0);
}

// inconsistent predicates; first predicate wins
declare function g(x: number): bool %checks (x === 0);
function g(x: number): bool %checks { return x === 1 }

if (g(x)) {
  (x: 0);
  (x: 1); // error
}

// declared function must be before regular function
function h(x: number): bool %checks { return x === 1 }
declare function h(x: number): bool %checks (x === 0);


//multiple declarations where at least one is a predicate is not allowed
declare function i(x: number): bool %checks (x === 0);
declare function i(x: string): bool %checks (x === "A");
function i(x: number): bool %checks { return x === 1 }

//multiple declarations where at least one is a predicate is not allowed
declare function j(x: number): bool;
declare function j(x: string): bool %checks (x === "A");

declare function k(x: string): bool %checks (x === "A");
declare function k(x: number): bool;
