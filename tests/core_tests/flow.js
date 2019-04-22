//@flow

declare function sideEffect(): boolean
declare var b: boolean

function f0(): string { // TODO?
  while (true) {
    return '';
  }
}

function f(): string { // TODO?
  while (true) {
    if (false) {
      return 'wot'
    }
  }
}

function loop0(): empty { // error
  while (true) {
    break;
  }
}

function loop1(): empty { // TODO: error
  a: while (true) {
    while (true) {
      break a;
    }
  }
}


function loop1_1(): empty { // ok
  a: while (true) {
    b: while (true) {
      break b;
    }
  }
}

function loop2(): empty { // ok
  a: while (true) {
    while (true) { }
  }
}

function loop3(): empty { // ok
  while (true) { }
}

function loop4(): empty { // ok
  while (sideEffect() || true) { }
}

function loop5(): empty { // TODO: ok
  while (true && true) { }
}

function loop6(): empty { // error
  while (true && false) { }
}

function loop7(): empty { // TODO: ok
  while (true || false) { }
}

function loop8(): empty { // ok
  if (b) {
    while (true) { }
  } else {
    while (true) { }
  }
}

function loop9(): empty { // error
  if (b) {
    while (true) { }
  }
}

function loop10(): empty { // ok
  l: while (true) {
    continue l
    break l // 'Unreachable' in Flow
  }
}

function for0(): empty { // TODO: ok
  for (; ;) { }
}

function for1(): empty { // TODO: ok
  for (; true;) { }
}

function for2(): empty { // TODO: ok
  for (; sideEffect() || true;) { }
}

function for3(): empty { // error
  for (; false;) { }
}

function for4(): empty { // error
  a: for (; ;) {
    break a
  }
}

function doWhile0(): empty { // TODO: ok
  do {

  } while (true)
}

function doWhile1(): empty { // TODO: ok
  do {

  } while (true && true)
}

function doWhile2(): empty { // TODO: ok
  do {

  } while (sideEffect() || true)
}

function doWhile3(): empty { // error
  do {
    break
  } while (true)
}

function doWhile4(): empty { // error
  l: do {
    while (true) {
      break l
    }
  } while (true)
}


// Tests with `if (<boolean-literal>)`

function if0(): empty { // TODO: ok
  if (true) {
    while (true) { }
  }
}

function if1(): empty { // error
  if (false) {
    while (true) { }
  }
}

function if2(): empty { // TODO: ok
  if (true) {
    do {

    } while (true)
  }
}

function if3(): empty { // ok
  while (true) {
    if (false) break
  }
}

function if4(): empty { // TODO: error
  a: while (true) {
    while (true) {
      if (true)
        break a
    }
  }
}

function if5(): empty { // ok
  a: while (true) {
    while (true) {
      if (false)
        break a
    }
  }
}
