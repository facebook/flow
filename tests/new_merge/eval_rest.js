// @flow

type D = $Diff<{ f: number, g: string }, { f: number }>;
declare var d: D;
declare export var d_: typeof d.g;

type R1 = $Rest<{| f: number, g: string |}, {| f: number |}>;
declare var r1: R1;
declare export var r1_: typeof r1.g;

type R2 = $Rest<{ f: number, g: string }, { f: number }>;
declare var r2: R2;
declare export var r2_: typeof r2.g;

type R3 = $Rest<{ f: number, g: string }, {| f: number |}>;
declare var r3: R3;
declare export var r3_: typeof r3.g;
