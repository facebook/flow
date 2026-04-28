declare class C { x: number; }
interface C { y: string; }
interface C { z: boolean; }

declare const c: C;
c.x as number;
c.y as string;
c.z as boolean;
c.x as string; // ERROR
c.y as boolean; // ERROR
c.z as number; // ERROR
