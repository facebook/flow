declare opaque type T;
declare var key: string;
declare var dict0: { [key: string]: () => T, ... };
dict0[key]();

declare var dict1: ?{ [key: string]: () => T, ... };
dict1?.[key]();

declare var dict2: ?{ [key: string]: () => T, ... };
dict2?.[key]?.();

declare var dict3: { [key: string]: <T>(T) => T, ... };
dict3[key]("");
