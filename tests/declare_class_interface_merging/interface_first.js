interface C { y: string; }
declare class C { x: number; }

declare const c: C;
c.x as number;
c.y as string;
c.x as string; // ERROR
c.y as number; // ERROR
