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
