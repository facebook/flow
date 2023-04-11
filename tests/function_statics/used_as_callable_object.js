function add(x: number, y: number) {
  return x + y;
}

function addWithBar(x: number, y: number) {
  return x + y;
}
addWithBar.bar = "bar";


type InexactCallableObj = {
  (number, number): number,
  bar: string,
  ...
};

type ExactCallableObj = {
  (number, number): number,
  bar: string,
};

declare class C {
  (n: number, m: number): number;
  bar: string;
};

interface I {
  (n: number, m: number): number;
  bar: string;
};


(add: InexactCallableObj); // error prop 'bar' missing
(add: ExactCallableObj); // error inexact obj
(add: C); // error object incompatible with class instance
(add: I); // error prop 'bar' missing

(addWithBar: InexactCallableObj); // okay
(addWithBar: ExactCallableObj); // error inexact obj
(addWithBar: C); // error object incompatible with class instance
(addWithBar: I); // okay
