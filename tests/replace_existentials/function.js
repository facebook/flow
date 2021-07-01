//@flow

function fn(a: *): number {
  return a + 1;
}

function fn1(a: *): number {
  return a + 1;
}
fn1(1);

function fn2(a: number): * {
  return a + 1;
}

function fn3<T: {x: number, ...}>(a: T): boolean {
  return a.x > 3
}
fn3<*>({x: 0, y: 1});

function fn4<T>(x: T): * {
  return x;
}

function unknown(x: *): number {
  return 1;
}

