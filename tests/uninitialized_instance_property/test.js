/* @flow */

/* EXPECTED TO ERROR */

class E1 {
  p: number; // PropertyNotDefinitivelyInitialized
}

class E2 {
  p1: number; // PropertyNotDefinitivelyInitialized
  p2: number; // PropertyNotDefinitivelyInitialized
}

class E3 {
  #priv; // PropertyNotDefinitivelyInitialized
}

class E4 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    if (true) return;
    this.p = 0;
  }
}

class E5 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    if (true) throw "";
    this.p = 0;
  }
}

class E6 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    {
      if (true) return;
      this.p = 0;
    }
  }
}

class E7 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    {
      if (true) throw "";
      this.p = 0;
    }
  }
}


class E8 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    if (true) {
      return;
    }
    this.p = 0;
  }
}

class E9 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    if (true) {
      throw "";
    }
    this.p = 0;
  }
}

class E10 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    if (true) {
      this.p = 0;
      return;
    }
  }
}

class E11 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    for (;;) {
      return;
    }
    this.p = 0;
  }
}

class E12 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    if (true) {
      for (;;) {
        return;
      }
      this.p = 0;
    } else {
      this.p = 0;
    }
  }
}

class E13 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    do {
      if (true) continue;
      this.p = 0;
    } while (true);
  }
}

class E14 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    do {
      if (true) break;
      this.p = 0;
    } while (true);
  }
}


class E15 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    do {
      if (true) return;
      this.p = 0;
    } while (true);
  }
}

class E16 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    do {
      if (true) throw "";
      this.p = 0;
    } while (true);
  }
}

class E17 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    this.p += 3; // ReadFromUninitializedProperty
  }
}

class E18 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    this.p; // ReadFromUninitializedProperty
  }
}

class E19 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    const x = this.p; // ReadFromUninitializedProperty
  }
}

class E20 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    if (this.p === 0) false; // ReadFromUninitializedProperty
  }
}

class E21 {
  p;
  constructor() {
    this.p; // ReadFromUninitializedProperty
    this.p = 0;
  }
}

class E22 {
  p: number; // PropertyNotDefinitivelyInitialized
  constructor() {
    this.p++; // ReadFromUninitializedProperty
  }
}

class E23 {
  p: number;
  constructor() {
    this.p++; // ReadFromUninitializedProperty
    this.p = 0;
  }
}

class E24 {
  constructor() {
    let x: number;
    let y: number = x; // ExpectError uninitialized variable is incompatible with number
  }
}

class E25 {
  p: number;
  constructor() {
    let x: number;
    this.p = x; // ExpectError uninitialized variable is incompatible with number
  }
}

class E26 {
  p;
  constructor() {
    this.p = this; // ThisBeforeEverythingInitialized
  }
}

class E27 {
  p;
  constructor() {
    if (this) {} // ThisBeforeEverythingInitialized
    this.p = 0;
  }
}

function f(o) {}
class E28 {
  p;
  constructor() {
    f(this); // ThisBeforeEverythingInitialized
    this.p = 0;
  }
}

class E29 {
  p;
  constructor() {
    const x = this; // ThisBeforeEverythingInitialized
    this.p = 0;
  }
}

class E30 {
  p;
  constructor() {
    this.m(); // MethodCallBeforeEverythingInitialized
    this.p = 0;
  }
  m(): void {}
}

class E31 {
  p1;
  p2;
  constructor() {
    this.p1 = 0;
    this.m(); // MethodCallBeforeEverythingInitialized
    this.p2 = 0;
  }
  m(): void {}
}

class E32 {
  p1: number; // PropertyNotDefinitivelyInitialized
  p2;
  constructor() {
    this.p2 = 0;
  }
}

class E33 {
  p;
  #p; // PropertyNotDefinitivelyInitialized
  constructor() {
    this.p = 0;
  }
}

class E34 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    if (true) {
      this.p = 0;
    } else if (false) {
      this.p = 0;
    }
  }
}

class E35 {
  p;
  constructor() {
    this.p = this.p; // ReadFromUninitializedProperty
  }
}

class E36 {
  p: {x: number}; // PropertyNotDefinitivelyInitialized
  constructor() {
    this.p.x = 0; // ReadFromUninitializedProperty
  }
}

class E37 {
  p1; // PropertyNotDefinitivelyInitialized
  p2;
  constructor() {
    while (this.p2 = this.p1) { // ReadFromUninitializedProperty
      this.p1 = 0;
    }
  }
}

class E38 {
  p: number; // PropertyNotDefinitivelyInitialized
  constructor() {}
}

class E39 {
  p1: number;
  p2: number;
  constructor() {
    this.p1 = this.p2; // ReadFromUninitializedProperty
    this.p2 = this.p1;
  }
}

class E40 {
  p: number; // PropertyNotDefinitivelyInitialized
  constructor() {
    switch (4) {
      case 0:
      case 1:
        if (true) {
          break;
        }
      default:
        this.p = 6;
        break;
    }
  }
}


class E41 {
  p: number; // PropertyNotDefinitivelyInitialized
  constructor() {
    switch (0) {
      case 0:
        this.p = 0;
        break;
      case 1:
        this.p = 0;
        break;
    }
  }
}

class E42 {
  p: number; // PropertyNotDefinitivelyInitialized
  constructor() {
    switch (0) {
      case 0:
        break;
      default:
        this.p = 0;
        break;
    }
  }
}

class E43 {
  p: number; // PropertyNotDefinitivelyInitialized
  constructor() {
    while (false) {
      this.p = 1;
    }
  }
}

class E44 {
  p: number; // PropertyNotDefinitivelyInitialized
  constructor() {
    for (;;) {
      this.p = 1;
      break;
    }
  }
}

class E45 {
  p: number; // PropertyNotDefinitivelyInitialized
  constructor() {
    let p: number;
    p = 10;
  }
}

class E46 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    label: {
      if (true) {
        break label;
      }
      this.p = 0;
    }
  }
}

class E47 {
  p: number; // PropertyNotDefinitivelyInitialized
  constructor() {
    do {
      if (true) {
        continue;
      }
      this.p = 0;
    } while (false);
  }
}

class E48 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    do {
      if (true) {
        break;
      }
      this.p = 0;
    } while (false);
  }
}

class E49 {
  p;
  constructor() {
    while (true) {
      this.p; // ReadFromUninitializedProperty
      break;
    }
    this.p = 0;
  }
}

class E50 {
  p;
  constructor() {
    if (true) {
      this.p; // ReadFromUninitializedProperty
    }
    this.p = 0;
  }
}

class E51 {
  p1;
  p2;
  constructor() {
    const a = (this.p2 = this.p1), // ReadFromUninitializedProperty
          b = (this.p1 = 0);
  }
}

class E52 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    const f = () => {
      this.p = 0;
    };
  }
}

class E53 {
  p; // PropertyNotDefinitivelyInitialized
  constructor() {
    function f() {
      this.p = 0;
    }
  }
}

class E54 {
  p: number;
  constructor() {
    this.m(this); // MethodCallBeforeEverythingInitialized, ThisBeforeEverythingInitialized
    this.p = 0;
  }
  m(o): void {}
}

class E55 {
  p: number;
  constructor() {
    this.m(this.p); // MethodCallBeforeEverythingInitialized, ReadFromUninitializedProperty
    this.p = 0;
  }
  m(x): void {}
}

class E56 {
  p: number;
  constructor() {
    this.m(this.m(0)); // MethodCallBeforeEverythingInitialized, MethodCallBeforeEverythingInitialized
    this.p = 0;
  }
  m(x): number {
    return x;
  }
}

class E57 {
  p: number;
  constructor() {
    this.m(this.p = 0); // MethodCallBeforeEverythingInitialized
  }
  m(x): number {
    return x;
  }
}

/* EXPECTED TO NOT ERROR */

class P1 {
  p: number = 0;
}

class P2 {
  p = 0;
}

class P3 {
  #priv = 0;
}

class P4 {
  constructor() {
    return;
  }
}

class P5 {
  constructor() {
    throw "";
  }
}

class P6 {
  p: number;
  constructor() {
    this.p = 0;
    this.p += 3;
  }
}

class P7 {
  p: number;
  constructor() {
    this.p = 0;
    this.p++;
  }
}

class P8 {}

class P9 {
  p: number;
  constructor() {
    this.p = 0;
  }
}

class P10 {
  #priv;
  constructor() {
    this.#priv = 0;
  }
}

class P11 {
  p1 = 0;
  p2;
  constructor() {
    this.p2 = this.p1;
  }
}

function g() {}
class P12 {
  p;
  constructor() {
    g();
    this.p = 0;
  }
}

class P13 {
  p;
  constructor() {
    if ((this.p = 0)) {}
  }
}

class P14 {
  p;
  constructor() {
    if (true) {
      this.p = 0;
    } else {
      this.p = 0;
    }
  }
}

class P15 {
  p;
  constructor() {
    if (true) {
      this.p = 0;
    } else if (false) {
      this.p = 0;
    } else {
      this.p = 0;
    }
  }
}

class P16 {
  p;
  constructor() {
    if (true) {
      this.p = 0;
    } else if (false) {
      if (false) {
        this.p = 0;
      } else {
        this.p = 0;
      }
    } else {
      this.p = 0;
    }
  }
}


class P17 {
  p1;
  p2;
  constructor() {
    this.p1 = this.p2 = 0;
  }
}

class P18 {
  p;
  constructor() {
    this.p = 0;
    const x = this.p;
  }
}

class P19 {
  p1;
  p2;
  constructor() {
    do {
      this.p1 = 0;
    } while (this.p2 = this.p1);
  }
}

class ParentP20 {
  p: number;
  constructor() {
    this.p = 0;
  }
}
class P20 extends ParentP20 {
  constructor() {
    super();
  }
}

class ParentP21 {
  p: number;
  constructor() {
    this.p = 0;
  }
}
class P21 extends ParentP21 {
}

class P22 {
  p1: number;
  p2: number;
  p3: number;
  p4: number;
  constructor() {
    if ((this.p1 = 1) != null) {}
    while ((this.p2 = 2) != null) {}
    for (; (this.p3 = 3) != null;) {}
    for (this.p4 = 4;;) {}
  }
}

class P23 {
  p: number;
  constructor() {
    do {
      this.p = 0;
    } while (false);
  }
}

function somethingThatThrows(): void { throw "the football"; }
class P24 {
  p: number;
  constructor() {
    try {
      this.p = 0;
      somethingThatThrows();
    } catch (err) {
      this.p = 0;
    }
  }
}

class P25 {
  p: number;
  constructor() {
    try {
      somethingThatThrows();
    } catch (err) {
      // do some stuff to handle the error
    } finally {
      this.p = 0;
    }
  }
}

class P26 {
  p1;
  p2;
  constructor() {
    const a = (this.p1 = 0),
          b = (this.p2 = this.p1);
  }
}

class P27 {
  p1;
  p2;
  constructor() {
    do {
      this.p1 = 0;
    } while (this.p2 = this.p1);
  }
}

class P28 {
  constructor() {
    this;
  }
}

class P29 {
  constructor() {
    this.m();
  }
  m(): void {}
}


class P30 {
  p;
  constructor() {
    this.p = 0;
    if (this) {}
  }
}

class P31 {
  p;
  constructor() {
    this.p = 0;
    this.m();
  }
  m(): void {}
}

class P32 {
  p;
  constructor() {
    this.p = 0;
    const x = this;
  }
}
