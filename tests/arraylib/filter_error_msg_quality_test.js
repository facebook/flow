// error due to bad subtyping
declare const str_arr: Array<string>;
str_arr.filter(s => s.length) as Array<boolean>;

// error due to unsupported refinements
declare const nullable_str_arr: Array<?string>;
nullable_str_arr.filter(s => !!s) as Array<string>;
