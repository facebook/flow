declare const x: string, y: number;
declare let a: number, b: string;
declare const c: boolean, d: number;

x as string; // OK
y as number; // OK
a as number; // OK
b as string; // OK
c as boolean; // OK
d as number; // OK
x as number; // ERROR
