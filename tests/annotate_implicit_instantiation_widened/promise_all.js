// @flow

declare function num(): Promise<number>;
declare function str(): Promise<string>;

async function foo() {
  // should not annotate the code below
  const [n, s] = await Promise.all([num(), str()]);
}
