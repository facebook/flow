type Curry = (<R>(() => R) => R) & (<A, R>((A) => R) => R);

declare class Lodash {
  curry: Curry;
  curry(func: Function): Function;
  noConflict(): Lodash;
}
