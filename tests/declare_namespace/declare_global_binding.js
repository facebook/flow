declare global {
  declare const foo: string;
}
global as empty; // ok: resolves to the global value from node.js libdef, instead of the namespace above
foo; // error: declare global doesn't affect global lookup for now
