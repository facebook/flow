import Thing from './umd_class';
import * as es from './umd_es';

// Type and namespace positions are allowed
declare const i: UmdEs.I;
i.x as number;
i.x as string; // ERROR

declare const mm: UmdCjs.M;
mm.y as string;
mm.y as number; // ERROR

declare const thing: UmdClass;
thing.foo() as number;
thing.foo() as string; // ERROR
declare const sub: UmdClass.SubThing;
sub.z as boolean;
sub.z as string; // ERROR

declare const s: SameFileA.S;
s as SameFileB.S;
s.s as string; // ERROR

// Value positions are not allowed in a module
UmdEs.f(); // ERROR
UmdCjs(); // ERROR
new UmdClass(); // ERROR
type EsNamespace = typeof UmdEs; // ERROR

// The globals and the modules agree
const t: UmdClass = new Thing();
({x: 1}) as es.I as UmdEs.I;

// Existing globals win over UMD globals
Shadowed as number;

// The first file wins between UMD globals
declare const a: Dup.A;
declare const b: Dup.B; // ERROR

// No global for a module with imports or an invalid declaration
declare const y: UmdImports.I; // ERROR
declare const sc: Script; // ERROR
