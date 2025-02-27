//@flow

type PropertyDescriptor<T> = number;

declare var y: {|a: number, b: number|};

Object.defineProperty(y, 'a', {writable: false});
Object.defineProperty(y, 'a', {value: "a"});
Object.defineProperty<number>(y, 'a', {writable: false});
Object.defineProperty<number>(y, 'b', {value: 42})
Object.defineProperty<number>(y, 'b', {value: "a"})
Object.defineProperty<string>(y, 'b', {value: "a"})
Object.defineProperty<string, string>(y, 'b', {value: "a"}) //targ arity mismatch
Object.defineProperty(y, 'c', {value: 42});
Object.defineProperty(y, 'c', {writable:false});

Object.defineProperties(x, {d: {value: 42}, e: {writable:false}, f:{get: () => 42}});
Object.defineProperties(y, {d: {value: 42}, a: {writable:false}, b:{get: () => "a"}});
Object.defineProperties<number>({}, {}); // targ arity mismatch

var z = {};
Object.create(z, {a: {get: () => 42},  b: {writable:true}});

Object.freeze<{}>({});
Object.freeze<{+a: number}>({a: 42, b: 42});
Object.freeze<{+a: number}>({a: 42, b: 42}).b; // b is hidden
Object.freeze<{+c: number}>({a: 42, b: 42}); // c does not exist
Object.freeze<number, number>({}); // targ arity misnatch
