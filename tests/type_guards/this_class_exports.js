// @flow

export opaque type Opaque: Base = Derived;

export class Base {
  isDerived(): this is Derived {
    return this instanceof Derived; // okay
  }

  isOpaque(): this is Opaque {
    return this instanceof Derived; // okay transparent in the defining file
  }

  isDerivedUnchecked(): this is Derived {
    return true; // error Base ~> Derived, but the guard is still recorded
  }
}

export class Derived extends Base {}

export class GenericBase<X> {
  isGenericDerived(): this is GenericDerived<X> {
    return this instanceof GenericDerived; // okay
  }
}

export class GenericDerived<X> extends GenericBase<X> {}

export class WithThisParam {
  isSub(this: WithThisParam): this is Sub {
    return this instanceof Sub; // okay
  }
}

export class Sub extends WithThisParam {}
