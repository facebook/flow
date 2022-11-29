// @flow

function test1() {
  declare function poly<T>((string) => void, T): T;
  declare function expectString(string): string;
  poly((v) => {}, expectString(3)); // Error: incompatible-call, but v can still be contextually typed
}
