// @flow

async function foo(): Promise<number => void> {
  const f: number => void = (n) => {}; // ok
  return (n) => {}; // ok
}

let f: () => Promise<(number) => void> = async () => (n) => {}; // ok
