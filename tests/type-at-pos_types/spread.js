// @flow

declare opaque type O;
declare var o: O;
type C = {| f: O; |}
class P<X> { p: X }
class D { g: O; }
type Ctor = Class<D>;

// Concrete - should be evaluated before normalization
type Obj1   = {w: O, ...{ x: O, y: O }, z: O};
type EObj2  = {|w: O, ...{| x: O, y: O |}, z: O|};
type C1     = {...C, o: O};
type C2     = {o: O, ...C};
type EC1    = {|...$Exact<C>, o: O|};
type EC2    = {|o: O, ...C|};
type Ctor1  = {...Ctor, o: O};
type Ctor2  = {o: O, ...Ctor};
type P1<T>  = {...P<T>, o: O};
type P2<T>  = {o: O, ...P<T>};
type N1     = {o: O, ...Number};
type Rec    = {o: O, ...Rec};  // Rec = empty
type Inter1 = {...Obj1} & {...C1};

// Unevaluated
type B1<T: {}, S: {}> = {...T, ...S, o: O};
type B2<T: {}> = {...T, o: O};
type B3<T: {}> = {o: O, ...T};
type B4<T: {}> = {o: O, ...T, oo: O};
type B5<T: {}> = {u: O, ...{ v: O, w: T }, x: O, ...T, y: O};
type EB1<T: {}, S: {}> = {|...T, ...S, o: O|};
type EB2<T: {}> = {|...T, o: O|};
type EB3<T: {}> = {|o: O, ...T|};
type EB4<T: {}> = {|o: O, ...T, oo: O|};
type EB5<T: {}> = {|u: O, ...{ v: O, w: T }, x: O, ...T, y: O|};
type PTA1<T: {}> = {...B2<T>, ...T};
type PTA2<T: {}> = {...T, ...B2<T>};
type EP1<T> = {|...P<T>, o: O|};
type EP2<T> = {|o: O, ...P<T>|};
type ECtor1 = {|...Ctor, o: O|};
type ECtor2 = {|o: O, ...Ctor|};
type PRec<X> = {o: O, ...PRec<X>};
type IP1<T: {}> = {...B1<T>} & {...B2<T>};  // --expand-type-aliases
type Nest1<T: {}> = {...{...T}};
type Nest2<T: {}> = {...{...{...T}}};
type UNest<T: {}> = {...T} | {...{...T}} | {...{...{...T}}};

declare var obj1: Obj1; obj1.z;
declare var eobj2: EObj2; eobj2.z;

declare var c1: C1; c1.o;
declare var c2: C2; c2.o;
declare var ec1: EC1; ec1.o;
declare var ec2: EC2; ec2.o;
declare var ctor1: Ctor1; ctor1.o;
declare var ctor2: Ctor2; ctor2.o;
declare var p1_num: P1<number>; p1_num.o;
declare var p2_num: P2<number>; p2_num.o;
declare var n1: N1; n1.o;
declare var rec: Rec; rec.o;
declare var inter1: Inter1; inter1.o;
