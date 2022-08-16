//@flow

var x: {
  a?: number,
  b?: string,
  d?: number,
  e?: void,
  f?: number,
} = {};
Object.defineProperty(x, 'a', {get: () => 42});
Object.defineProperty(x, 'b', {value: "a"})
Object.defineProperties(x, {d: {value: 42}, e: {writable:false}, f:{get: () => 42}});

var z = {};
Object.create(z, {a: {get: () => 42},  b: {writable:true}});
