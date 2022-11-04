// @flow

async function foo(): Promise<number => void> {
  const f: number => void = (n) => {}; // ok
  return (n) => {}; // ok
}

let f: () => Promise<(number) => void> = async () => (n) => {}; // ok

async function bar(): Promise<number => void> {
  return Promise.resolve((n) => {}); // ok
}
