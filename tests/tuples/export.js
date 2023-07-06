type A = [1, 2];
type B = [d?: 4];
export type T = [a: 0, ...A, c?: 3, ...B];
