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

// Passes


// do not lint on props initialized at declaration

class P1 {
  p: number = 0;
}

class P2 {
  p = 0;
}
