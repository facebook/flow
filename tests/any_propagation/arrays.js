//@flow

declare const noop : <T>(arr: Array<Array<T>>) => void;
declare const arr : Array<Array<?string>>;
let new_arr = [];
arr.forEach(x => new_arr.push(x));
new_arr = new_arr.filter(Boolean);
noop<string>(new_arr);
