// @flow

(Object.assign.apply(null, [({}: {a?: number, b?: string}), {a: 1}, {b: 'foo'}]): {a?: number, b?: string});
(Object.assign.apply(null, [({}: {a?: number, b?: string}), {a: 1}, {b: 2}]): {a?: number, b?: string}); // error
(Object.assign.apply(({}: {a?: number, b?: string}), {a: 1}, {b: 'foo'}): {a?: number, b?: string}); // error

(Object.assign.call(({}: {a?: number, b?: string}), [{a: 1}, {b: 'foo'}]): {a?: number, b?: string}); // error
(Object.assign.call(({}: {a?: number, b?: string}), {a: 1}, {b: 'foo'}): {a?: number, b?: string});
(Object.assign.call(({}: {a?: number, b?: string}), {a: 1}, {b: 2}): {a?: number, b?: string}); // error

(Object.assign.length : number);
(Object.assign.length : string); // error

(Object.assign.name : number); // error
(Object.assign.name : string);
