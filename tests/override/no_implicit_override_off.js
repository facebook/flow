// With `no_implicit_override` off (the default), a concrete subclass member
// that shadows an inherited member without `override` is silently allowed.
// The implicit-override check — the one that would require the modifier on
// such shadowing members — only runs when the option is on. This fixture
// uses a plain `.js` with concrete classes so the check is reached and
// then short-circuited by the option being off; ambient sigs (`declare
// class`, `.flow`, `declare module`) skip the check unconditionally and
// wouldn't exercise this path.

class Base {
  meth(): void {}
  field: string = "base";
}

class Sub extends Base {
  meth(): void {} // OK: implicit override allowed under default config
  field: string = "sub"; // OK
}

new Sub().meth();
new Sub().field as string;
