//@flow

declare var bool: boolean

function f1() {
  while (true) {
    let a
    if (bool) {
      a = 5
    } else {
      break
    }
    ; (a: number) // ok
  }
}

function f2() {
  a: while (true) {
    let a
    if (bool) {
      a = 5
    } else {
      break a;
    }
    ; (a: number) // ok
  }
}

function f3() {
  while (true) {
    let a
    if (bool) {
      a = 5
    } else {
      continue
    }
    ; (a: number) // ok
  }
}

function f4() {
  l: while (true) {
    let a
    if (bool) {
      a = 5
    } else {
      continue l
    }
    ; (a: number) // ok
  }
}

function f5() {
  while (true) {
    let a
    if (bool) {
      a = 5
    } else {
      if (bool) break
    }
    ; (a: number) // error
  }
}

function f5() {
  l: while (true) {
    let a
    if (bool) {
      a = 5
    } else {
      if (bool) continue l
    }
    ; (a: number) // error
  }
}

function f6() {
  a: while (true) {
    let a
    if (bool) {
      a = 5
    } else {
      l: break l;
    }
    ; (a: number) // error
  }
}