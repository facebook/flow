//@flow

export var a: $TEMPORARY$number<'a'> = 42;

export var b: $TEMPORARY$number<1, 1> = 42;

export const c = {['a' + 'b']: 42};

export const d = [...c];

export const e = (d += d);

export const f = class {};

export const u = 42n;

export const w = /foo/;

export const x = <f />;

function testit(arg: any): boolean %checks {
  return (arg: boolean);
}

export const g = testit;
