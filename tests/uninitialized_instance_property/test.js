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
