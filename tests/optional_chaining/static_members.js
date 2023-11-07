// @flow

type X = {foo: number};
type Y = {bar: X, baz: ?X};

declare var x1: ?X;
declare var x2: X;

declare var y1: ?Y;
declare var y2: Y;

x1?.foo as empty;
x2?.foo as empty;

y1?.bar?.foo as empty;
y2?.bar?.foo as empty;
y1?.baz?.foo as empty;
y2?.baz?.foo as empty;

y1?.bar.foo as empty;
y2?.bar.foo as empty;
y1?.baz.foo as empty;
y2?.baz.foo as empty;

y1.bar?.foo as empty;
y2.bar?.foo as empty;
y1.baz?.foo as empty;
y2.baz?.foo as empty;

(y1?.bar).foo as empty;
(y2?.bar).foo as empty;
(y1?.baz).foo as empty;
(y2?.baz).foo as empty;

const a: any | null = 1337;
const b: string | null = a?.a;
const c: ?any = 1337;
const d: string | null = c?.c;
