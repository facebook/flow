class A {
  abstract static create(): this;
  abstract static n(): number;
  abstract clone(): this;
  abstract s(): string;

  concreteClone(): this {
    return this.clone();
  }
}

class AbstractSupers extends A {
  static indirectCreate(): this {
    super.n = () => 5; //ng
    let k = super.create; //ng
    return super.create(); //ng
  }
  indirectClone(): this {
    super.s = () => "a string"; //ng
    let k = super.clone; //ng
    return super.clone(); //ng
  }
  indirectConcreteClone(): this {
    return super.concreteClone();
  }
}

class ImplA extends A {
  static create(): this {
    return new this;
  }
  clone(): this {
    return this;
  }
}

class NonabstactSupers extends ImplA {
  static indirectCreate(): this {
    return super.create();
  }
  indirectClone(): this {
    return super.clone();
  }
}

/*
// The extended class, ImplA, is typed as a non-abtract subtype of A, so `clone` and
// `create` are available on `super`. For `this` returns types, this use case
// is blocked by https://github.com/facebook/flow/issues/4240.
class C3 extends (ImplA: Class<A>) {
  static indirectCreate(): this {
    return super.create(); //ok
  }
  indirectClone(): this {
    return super.clone(); //ok
  }
}
*/
