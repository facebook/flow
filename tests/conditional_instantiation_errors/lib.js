declare type Flatten<A, D extends number> = {
  done: A,
  recur: A extends ReadonlyArray<infer Inner>
    ? Flatten<Inner, [-1, 0, 1][D]>
    : A,
}[D extends -1 ? 'done' : 'recur'];

declare type TrueBranch<T> = T extends number ? [number][T] : string;
declare type FalseBranch<T> = T extends string ? string : [number][T];
