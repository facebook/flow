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

function leak(f) {
  f.y; // error
  f.x; // error
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
  constructor(b) {
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
  constructor(leaked_this) {
    leaked_this.foo()
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
  constructor(closure_leaking_this) {
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
  constructor(closure_leaking_this) {
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
