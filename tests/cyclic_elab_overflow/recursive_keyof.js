// @flow
type Config = {
  [key: Key]: number,
};
declare const config: Config;
type Key = keyof typeof config;
declare const key: Key;
key.includes('x');
