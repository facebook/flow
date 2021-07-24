// @flow

function foo() {
    while(true) { break; }
}

function bar() {
    L: do { continue L; } while(false)
}

function breakContinue(b: boolean) {
  while (true) {
    if (b) {
      break;
    } else {
      continue;
    }
    break; // Error - unreachable
  }
}

function continueBreak(b: boolean) {
  while (true) {
    if (b) {
      continue;
    } else {
      break;
    }
    break; // Error - unreachable
  }
}

function breakContinueLabeled(b: boolean) {
  while (true) {
    if (b) {
      break;
    } else {
      continue;
    }
    break; // Error - unreachable
  }
}

function continueBreakLabeled(b: boolean) {
  l: while (true) {
    if (b) {
      continue l;
    } else {
      break l;
    }
    break; // Error - unreachable
  }
}

function moreBreakContinue(b: boolean) {
  let x: null | number = 3;
  while (x === null) {
    if (b) {
      x = null;
      break;
    } else {
      continue;
    }
  }
  (x: number); // Error - null incompatible with number
}

function differentLabels(b: boolean) {
  a: for (;;) {
    b: while (true) {
      if (b) {
        continue a;
      } else {
        break b;
      }
      break; // Don't handle this because of different labels
    }
  }
}
