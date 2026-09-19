// @flow
export type Item = {
  label: string,
};
export type Cfg = {
  [key: K]: Item,
};
export const CONFIG: Cfg = {
  a: {label: 'a'},
  b: {label: 'b'},
};
export type K = keyof typeof CONFIG;
export type T = K;
export function isA(key: K): boolean {
  return key.includes('a');
}
