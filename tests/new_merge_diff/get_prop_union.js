// @flow

declare class C1 {
  f: number;
}

declare class C2 {
  f: string;
}

declare var c: C1 | C2;

/* $FlowExpectedError[incompatible-type] this is a known issue with typeof parts
 * running against eachother. */
declare export var x: typeof c.f;
