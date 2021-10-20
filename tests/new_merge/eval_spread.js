// @flow

type O1 = { f: number, g: string, ... };
type O2 = {| f: number, g: string |};

type P1 = { ...O1, f: string };
type P2 = { ...O2, f: string };
// $FlowExpectedError[incompatible-exact]
type P3 = {| ...O1, f: string |};
type P4 = {| ...O2, f: string |};

declare export var p1: P1;
declare export var p2: P2;
declare export var p3: P3;
declare export var p4: P4;

export const p1_f = p1.f;
export const p2_f = p2.f;
export const p3_f = p3.f;
export const p4_f = p4.f;

export const p1_g = p1.g;
export const p2_g = p2.g;
export const p3_g = p3.g;
export const p4_g = p4.g;
