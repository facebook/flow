// ES `#foo` is a separate identity from public `foo`. `override #foo` against
// a base whose public `foo` happens to share the bare string must NOT match —
// the not-inherited check should fire OverrideOfNonInheritedMember.
// Regression test for the private-name collision bug.

class Base {
  foo(): void {} // public
}

class Sub extends Base {
  override #foo(): void {} // ERROR: `#foo` is a separate identity from Base's public `foo`
}

new Sub();
