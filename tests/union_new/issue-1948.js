// @flow
type Action = {
  type: 'A' | 'B',
  val: number
}

type Thunk = (n: number) => number

type A = Thunk | Action
type B = Action | Thunk

const a1 = (n: number): A => ((n: number): number => n)
const b1 = (n: number): B => ((n: number): number => n)

const a2 = (n: number): A => ((n) => n);
const b2 = (n: number): B => ((n) => n);

const a2_ = (n: number): Thunk => ((n) => n);

const a3: A = n => n;
const b3: B = n => n;

const a3_: Thunk = n => n;

const a4: A = (n: number) => n;
const b4: B = (n: number) => n;

const a4_: Thunk = (n: number) => n;

const a5: A = (n: number): number => n;
const b5: B = (n: number): number => n;

const a6 = (n: number): A => ({type: 'A', val: n});
const b6 = (n: number): B => ({type: 'B', val: n});
