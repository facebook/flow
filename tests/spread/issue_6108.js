// @flow

type A = {
  a: number
};

type B = {
  a: string
};

const testFn = (input: A): B => ({...input, a: 'override'});
