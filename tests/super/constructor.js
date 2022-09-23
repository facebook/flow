class A { x: number; }

class B {
  x: number;
  constructor() {
    this.x; // OK
  }
}

class C extends A { }

class D extends A {
  y: number;
  constructor() {
    this.y; // error (no super call)
    this.x; // error (no super call)
  }
}

class E extends A {
  y: number;
  constructor() {
    super();
    this.y; // OK
    this.x; // OK
  }
}

function leak(f: F) {
  f.y; // OK (errors at leaked site)
  f.x; // OK (errors at leaked site)
}
class F extends A {
  y: number;
  constructor() {
    leak(this); // error (no super call yet)
    super();
    this.y;
    this.x;
  }
}

class G extends A {
  constructor(b: mixed) {
    super.x; // error (no super call)
  }
}

class H extends A {
  y: number;
  constructor() {
    if (Math.random() < 0.5)
      super();
    else
      super();
    this.y; // OK
    this.x; // OK
  }
}

class I_ {
  constructor(leaked_this: I_) {
    leaked_this.foo() // OK (errors at leaked site)
  }
  foo() { }
}
class I extends I_ {
  constructor() {
    super(this); // error (no super call yet)
  }
}

class J__ { }
class J_ extends J__ {
  constructor(closure_leaking_this: () => void) {
    closure_leaking_this();
    super();
  }
  foo() { }
}
class J extends J_ {
  constructor() {
    super(() => this.foo()); // error (no super call yet)
    // The reason for this error is that super constructor could call the
    // closure and therefore access this before calling its own super
    // constructor (as shown above). The only safe thing to do in the super
    // constructor is to save the closure so that it can be called later, after
    // initialization is done (as shown below).
  }
}

class K_ {
  closure_leaking_this: () => void;
  constructor(closure_leaking_this: () => void) {
    this.closure_leaking_this = closure_leaking_this;
  }
  foo() { }
}
class K extends K_ {
  constructor() {
    super(() => { if (_this) _this.foo() }); // OK
    var _this = this;
    this.closure_leaking_this();
  }
}

// We enforce that constructors cannot have non-void returns
class L_ {
  constructor() {
    return false; // error: boolean ~> void
  }
}
class L extends L_ {
  constructor() {
    let x: boolean = super(); // error: void ~> boolean
    return x; // error: boolean ~> void
  }
}
(new L_(): L_);
(new L(): L);

class M_ {
  constructor() {
    return {foo: 'foo'}; // error: object ~> void
  }
}
class M extends M_ {
  constructor() {
    return super(); // OK
  }
}
(new M_(): M_);
(new M(): M);

class N_ {
  constructor(): this { // error: this ~> void
    let x = this;
    return x;
  }
}
class N extends N_ {
  constructor() {
    return super(); // error: N ~> void
  }
}
(new N_(): N_);
(new N(): N);
