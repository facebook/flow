type O1 = { p: number, q: number, ... }
declare const o1: O1;
var {p:p1, ...o1_rest} = o1;
o1_rest as { q: number, ... }; // ok
o1_rest.x = 0; // error: rest result is sealed

type O2 = {p: number, q: number}
declare const o2: O2;
var {p:p2, ...o2_rest} = o2;
o2_rest as {q: number}; // ok
o2_rest.x = 0; // error: rest result is sealed

var o3 = { p: 0, q: 0 };
var {p:p3, ...o3_rest} = o3;
o3_rest as {q: number}; // ok
o3_rest.x = 0; // error: rest result is sealed

type RestChoice =
  | Readonly<{kind: 'a', a: number}>
  | Readonly<{kind: 'b', b: string}>
  | Readonly<{kind: 'c', c: boolean}>;
type RestProps = Readonly<{common: number, ...RestChoice}>;
type InternalRestProps = Readonly<{...RestProps, extra: string}>;

function readRestChoice(
  {common, extra, ...rest}: InternalRestProps,
): number | string | boolean {
  switch (rest.kind) {
    case 'a': return rest.a;
    case 'b': return rest.b;
    case 'c': return rest.c;
  }
}

function checkRestChoice({common, extra, ...rest}: InternalRestProps) {
  switch (rest.kind) {
    case 'a': rest.a as string; break; // error: number
    case 'b': rest.b as boolean; break; // error: string
    case 'c': rest.c as number; break; // error: boolean
  }
}

function repeatRest() {
  let value = {kind: 'a', a: 1};
  while (true) {
    const {...rest} = value;
    value = rest;
  }
}
