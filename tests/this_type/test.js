// Examples with `this` types

class Base {
  foo(): this { return this; }
  qux(): Base { return new Base2; }

  bar(): this { return this; }
  bar_caller(): this { return this.bar(); }

  corge(that: this) { }
  grault(that: Base) { }
}

class Inherit extends Base { }

class Override extends Base {
  foo(): this { return this; } // OK
  qux(): this { return this; } // OK, too

  bar(): Override { return new Override; } // error (cf. OK above)
                                             // see exploit below

  corge(that: this) { } // error
                        // see exploit below
  grault(that: this) { } // error, too (only variance error with new-generics)
}

class InheritOverride extends Override { }

(new Inherit().foo(): Base);
(new Inherit().foo(): Inherit); // OK (cf. error above)
((new Inherit(): Base).foo(): Base);
(new Override().foo(): Base);
(new Override().foo(): Override); // OK
((new Override(): Base).foo(): Base);

(new InheritOverride().bar_caller(): InheritOverride); // exploits error above

(new Override(): Base).corge(new Base()); // exploits error above
