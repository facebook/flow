//@flow

const x = {foo: 3};
Object.keys(x).map(k => { return {[k]: k} }); // No error

const y = {foo: 3, bar: 3};
Object.keys(y).map(k => { return {[k]: k} }); // No error
