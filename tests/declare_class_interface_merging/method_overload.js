declare class C { m(x: number): void; }
interface C { m(x: string): void; }

declare const c: C;
c.m(1);     // OK
c.m("hi");  // OK
c.m(true);  // ERROR
