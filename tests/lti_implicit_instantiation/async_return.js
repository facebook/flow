//@flow

async function f() {
  return await (null as any);
}

async function g() {
  return await [][0];
}
