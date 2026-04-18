// @flow

// Basic property merging: both `s` and `b` should be visible
interface MergedA { s: string; }
interface MergedA { b: number; }
declare var merged_a: MergedA;
merged_a.s as string; // ok
merged_a.b as number; // ok
merged_a.s as number; // error: string ~> number
merged_a.b as string; // error: number ~> string

// Same prop, same type: should be allowed
interface SameProp { x: string; }
interface SameProp { x: string; }
declare var same_prop: SameProp;
same_prop.x as string; // ok

// Same prop, different types: should error (unification failure)
interface ConflictProp { x: string; }
interface ConflictProp { x: number; } // error: number ~> string

// Method overload merging
interface MergedMethods { foo(s: string): string; }
interface MergedMethods { foo(n: number): number; bar(): boolean; }
declare var merged_methods: MergedMethods;
merged_methods.foo("") as string; // ok
merged_methods.foo(0) as number; // ok
merged_methods.bar() as boolean; // ok

// Three-way merge
interface ThreeWay { first: string; }
interface ThreeWay { second: number; }
interface ThreeWay { third: boolean; }
declare var three_way: ThreeWay;
three_way.first as string; // ok
three_way.second as number; // ok
three_way.third as boolean; // ok
three_way.first as number; // error: string ~> number
