// Instance and static sides are checked independently. An instance member
// cannot satisfy a static `override` and vice versa.

class Base {
  static s(): void {}
  i(): void {}
}

class GoodStatic extends Base {
  static override s(): void {} // OK
}

class StaticOverridesNothing extends Base {
  static override missing(): void {} // ERROR: not declared in `Base`
}

class InstanceOverridesStatic extends Base {
  override s(): void {} // ERROR: instance side has no `s`
}

class StaticOverridesInstance extends Base {
  static override i(): void {} // ERROR: static side has no `i`
}
