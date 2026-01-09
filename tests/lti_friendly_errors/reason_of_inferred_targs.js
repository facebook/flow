const p = new Promise((resolve) => resolve(""));
(p: Promise<string>); // error

declare function magicCall1<T>(x: T => mixed): void;
declare function magicCall2<T = number>(x: T => mixed): T;

magicCall1(s => (s: string)); // error
magicCall2(s => (s: string)); // error
magicCall1(s => s.abcd); // error
magicCall2(s => s.abcd); // error
