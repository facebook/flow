declare var x: { x: { foo: string } };
declare var y: { x: { bar: number } };
x = y; // 2 errors: `foo` not found in y.x; `bar` not found in x.x
