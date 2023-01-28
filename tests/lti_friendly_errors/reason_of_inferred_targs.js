const p = new Promise((resolve) => resolve(""));
(p: Promise<string>); // error

declare function magicCall1<T>(T => mixed): void;
declare function magicCall2<T = number>(T => mixed): T;

magicCall1(s => (s: string)); // error
magicCall2(s => (s: string)); // error
magicCall1(s => s.abcd); // error
magicCall2(s => s.abcd); // error
