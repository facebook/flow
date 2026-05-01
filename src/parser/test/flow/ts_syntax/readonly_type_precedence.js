type F1 = readonly string[] | string;
type F2 = string | readonly string[] | undefined;
type F3 = string | readonly string[] | number;
type F4 = readonly string[] & {length: number};
type F5 = readonly [number, string] | null;
type F6 = readonly string[] extends unknown ? 1 : 2;
type F7 = readonly [number, string] extends readonly any[] ? true : false;
