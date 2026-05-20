// Regression test for polymorphic `this` rebinding through namespace-member
// references in interface `extends`. The namespace's `types_tmap` stores the
// raw [ClassT(ThisInstanceT _)] shape for class-like members, but the
// extends consumer used to canonicalize via [mk_instance], destroying the
// polymorphic-`this` form. Fixed by detecting namespace-member class-like
// refs at the convert layer and routing through [TypeUtil.this_typeapp].

declare namespace NS {
  declare export interface NSCloneable {
    clone(): this;
  }
}

interface NSChild extends NS.NSCloneable {
  extra: string;
}

declare const nsc: NSChild;
nsc.clone() as NSChild;            // OK — `this` rebinds through namespace
nsc.clone().extra as string;       // OK — `extra` accessible after rebinding

// Class implementing a namespace-member interface should also rebind `this`.
class NSGoodImpl implements NS.NSCloneable {
  clone(): this { return this; }   // OK
}

class NSBadImpl implements NS.NSCloneable {
  clone(): NS.NSCloneable { return this; }  // ERROR — must return `this`
}
