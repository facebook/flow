declare const inter1: {foo: string, ...} & {bar: number, ...};
Object.assign(inter1, {foo: ''}); // ok
Object.assign(inter1, {bar: 3}); // ok
Object.assign(inter1, inter1); // ok
Object.assign(inter1, {foo: '', bar: 3}); // error: should be ok
Object.assign(inter1, {foo: 3});; // error
Object.assign(inter1, {bar: ''});; // error
