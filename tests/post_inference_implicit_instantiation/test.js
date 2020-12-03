//@flow
declare var arg: any;
import type {Covariant, Contravariant, Invariant} from './types';
declare function f1<T>(Covariant<T>): Covariant<T>; // Contra param, Co return
declare function f2<T>(Covariant<T>): Contravariant<T>; // Contra param, Contra return
declare function f3<T>(Covariant<T>): Invariant<T>; // Contra param, Inv return
declare function f4<T>(Contravariant<T>): Covariant<T>; // Co param, Co return
declare function f5<T>(Contravariant<T>): Contravariant<T>; // Co param, Contra return
declare function f6<T>(Contravariant<T>): Invariant<T>; // Co param, Inv return
declare function f7<T>(Invariant<T>): Covariant<T>; // Inv Param, Co return
declare function f8<T>(Invariant<T>): Contravariant<T>; // Inv Param, Contra Return
declare function f9<T>(Invariant<T>): Invariant<T>; // Inv Param, Inv Return
f1(arg);
f2(arg);
f3(arg);
f4(arg);
f5(arg);
f6(arg);
f7(arg);
f8(arg);
f9(arg);

declare function missing<T>(): void; // Missing param and return
missing();


declare function InterdependentBounds<T, U: Covariant<T>>( // Follow bounds in each position we see U
 Covariant<U>,
): Covariant<U>;

InterdependentBounds(arg);

declare function NameOverride<T>(<T>(T) => T, T): <T>(T) => T; // Contra Param, no return 
NameOverride(arg);

declare function RestParam<T>(...$ReadOnlyArray<Contravariant<T>>): void; // Co param, no return
RestParam(arg);

declare var Overloaded: (<T>(null) => T) & (<T>(T) => T);
Overloaded(null);
Overloaded(3); // Need a not-any value so the second branch is hit

declare var Union: (<T>(T) => T) | (<T>(null) => T);
Union(arg);
