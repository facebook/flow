type B = {a: string}
type A = {
  ...B,
  [K in V]: number,
}
