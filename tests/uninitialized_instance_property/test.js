/* @flow */

// Errors

// lint on props not initialized at declaration

class E1 {
  p: number;
}

class E2 {
  p1: number;
  p2: number;
}

// lint on private fields not initialized at declaration

class E3 {
  #priv;
}

// Passes


// do not lint on props initialized at declaration

class P1 {
  p: number = 0;
}

class P2 {
  p = 0;
}

// do not lint on private fields initialized at declaration

class P3 {
  #priv = 0;
}
