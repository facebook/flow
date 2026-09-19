// @flow
type Config = {
  [key: Key]: number,
};
declare const config: Config;
type Key = keyof typeof config; // ERROR: expected recursive-definition error
declare var key: Key;
export type T = typeof key.includes;
