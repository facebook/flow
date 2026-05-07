declare opaque type T;
declare const key: string;
declare const dict0: { [key: string]: () => T, ... };
dict0[key]();

declare const dict1: ?{ [key: string]: () => T, ... };
dict1?.[key]();

declare const dict2: ?{ [key: string]: () => T, ... };
dict2?.[key]?.();

declare const dict3: { [key: string]: <T>(T) => T, ... };
dict3[key]("");

declare const dict4: { [key: 'a' ]: () => {...} } | void;
declare const b: 'b';
dict4[b]();
