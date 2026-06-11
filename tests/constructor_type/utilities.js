// ConstructorParameters / InstanceType

class Foo {
    x: number;
    constructor(x: number, y: string) { this.x = x; }
}

type FooParams = ConstructorParameters<typeof Foo>;
const fp1: FooParams = [1, "s"]; // OK
const fp2: FooParams = [1, 2]; // ERROR: number incompatible with string

type FooInstance = InstanceType<typeof Foo>;
declare const fi: FooInstance;
fi.x as number; // OK
fi as Foo; // OK

// With constructor type
type CtorWithTwoArgs = new (a: number, b: boolean) => {v: string};
type CWTA_Params = ConstructorParameters<CtorWithTwoArgs>;
const cp1: CWTA_Params = [1, true]; // OK
const cp2: CWTA_Params = ["bad", true]; // ERROR: string incompatible with number

type CWTA_Instance = InstanceType<CtorWithTwoArgs>;
declare const ci: CWTA_Instance;
ci.v as string; // OK

// Non-constructor types should be rejected by the constraint
type Bad1 = ConstructorParameters<string>; // ERROR: string is not a constructor
type Bad2 = InstanceType<() => void>; // ERROR: function is not a constructor

// Overloaded constructors (intersection of FunTs). Two things exercised:
// (a) direct `new` dispatches both overloads (regression check for
//     [method-unbinding]: the `this` binding is normalized on every leaf
//     of the intersection so neither overload is rejected at call time);
// (b) `ConstructorParameters` reaches an overloaded construct sig.
// NOTE: TS-style inference picks the LAST overload for `infer Args`, so
// in TS [1] errors and ["s"] is OK. Flow currently picks the first
// overload — a divergence to be addressed alongside Flow's broader
// overload-handling story (call dispatch, structural subtype, infer)
// rather than as a one-off here.
declare class Overloaded {
    x: number;
    constructor(x: number): void;
    constructor(x: string): void;
}
new Overloaded(1); // OK — number overload, no method-unbinding error
new Overloaded("s"); // OK — string overload, no method-unbinding error
new Overloaded(true); // ERROR: neither overload accepts boolean
type OverloadedParams = ConstructorParameters<typeof Overloaded>;
const op1: OverloadedParams = [1]; // OK in Flow (first overload); TS would error
const op2: OverloadedParams = ["s"]; // ERROR in Flow (first overload); TS would accept
type OverloadedInstance = InstanceType<typeof Overloaded>;
declare const oi: OverloadedInstance;
oi.x as number; // OK — InstanceType<typeof Overloaded> is Overloaded

// Generic / polymorphic construct signature. Same point: no method-unbinding
// error, the polytype is normalized through. Verifies that InstanceType
// preserves the Box<T> shape: with no inference context, T resolves to its
// top bound (unknown), so the instance is Box<unknown>.
type Box<T> = { v: T };
type GenericCtor = new <T>(x: T) => Box<T>;
type GenericInstance = InstanceType<GenericCtor>;
declare const gi: GenericInstance;
gi.v as unknown; // OK — GenericInstance has a `v` property

// Interface with quoted "new" method — should be a regular property, not a construct sig
interface QuotedNew {
    "new"(): number;
}
declare const q: QuotedNew;
q["new"]() as number; // OK — "new" is just a method
new q(); // ERROR: not a constructor

// Optional `new?()` method — also a regular (optional) property, not a construct sig
interface OptionalNew {
    new?(): number;
}
declare const o: OptionalNew;
new o(); // ERROR: not a constructor (no inst_construct_t slot)

// Bare class with no explicit constructor — relies on the synthesized default
class Bare {}
type BareParams = ConstructorParameters<typeof Bare>;
const bp: BareParams = []; // OK — synthesized default ctor takes no args
const bp_bad: BareParams = [1]; // ERROR: default ctor takes no args
type BareInstance = InstanceType<typeof Bare>;
declare const bi: BareInstance;
bi as Bare; // OK — InstanceType<typeof Bare> is Bare

// Derived class with no explicit constructor — relies on inherited constructor
class Base2 {
    x: number;
    constructor(x: number) { this.x = x; }
}
class Derived extends Base2 {}
type DerivedParams = ConstructorParameters<typeof Derived>;
const dp: DerivedParams = [1]; // OK — inherited from Base2
const dp_bad: DerivedParams = ["s"]; // ERROR: inherited ctor wants number
type DerivedInstance = InstanceType<typeof Derived>;
declare const di: DerivedInstance;
di.x as number; // OK — Derived's instance has Base2's `x: number`

// Declared (non-inline) interface with a construct signature — the canonical
// shape, e.g. `interface FooConstructor { new(): Foo }`. Goes through the
// type_annotation.ml [add_interface_properties] path rather than the inline
// [new (...) => T] path covered above.
interface MyCtor {
    new(x: number): {v: string};
}
type MyCtorParams = ConstructorParameters<MyCtor>;
const mcp1: MyCtorParams = [1]; // OK
const mcp2: MyCtorParams = ["bad"]; // ERROR: string incompatible with number
type MyCtorInstance = InstanceType<MyCtor>;
declare const mci: MyCtorInstance;
mci.v as string; // OK

// Interface that inherits its construct signature from `extends` parent.
interface ParentCtor {
    new(x: number): {tag: "parent"};
}
interface ChildCtor extends ParentCtor {}
declare const cc: ChildCtor;
const inheritedInst = new cc(1); // OK — construct sig inherited from Parent
inheritedInst.tag as "parent"; // OK
type ChildCtorParams = ConstructorParameters<ChildCtor>;
const ccp1: ChildCtorParams = [1]; // OK — inherited
const ccp2: ChildCtorParams = ["bad"]; // ERROR: string incompatible with number

// Interface with its OWN construct sig that ALSO inherits one from `extends`.
// Own and inherited sigs are merged as overloads (derived first, base after);
// both must dispatch. Regression check: before the [collect_construct_ts]
// walk-on-own change, the parent's sig was silently dropped when the child
// had its own.
interface ParentBoth {
    new(x: number): {kind: "parent"};
}
interface ChildBoth extends ParentBoth {
    new(x: string): {kind: "child"};
}
declare const cb2: ChildBoth;
const cb2_p = new cb2(1); // OK — inherited Parent overload
cb2_p.kind as "parent"; // OK
const cb2_c = new cb2("hi"); // OK — own Child overload
cb2_c.kind as "child"; // OK
new cb2(true); // ERROR: neither overload accepts boolean
// ConstructorParameters / InstanceType still pick the FIRST overload —
// the existing Flow-vs-TS divergence documented on `OverloadedParams` above.
// Collection order is [own, inherited], so "first" is Child's [string].
type ChildBothParams = ConstructorParameters<ChildBoth>;
const cbb1: ChildBothParams = ["hi"]; // OK in Flow (first = Child); TS picks Parent and would error
const cbb2: ChildBothParams = [1];    // ERROR in Flow (first = Child wants string); TS would accept

// Regression for nested-intersection flatten in [combine_construct_ts]: a child
// with TWO own overloads (stored as one [IntersectionT] in [inst_construct_t])
// plus an inherited sig would otherwise read back as
// [IntersectionT(IntersectionT(o0, o1), inh)], which [ty_normalizer]'s
// one-level [InterRep.members] unwrap can't render correctly. All three
// overloads must dispatch.
interface ChildTwoOwn extends ParentBoth {
    new(x: string): {kind: "child-s"};
    new(x: boolean): {kind: "child-b"};
}
declare const cto: ChildTwoOwn;
const cto_p = new cto(1); // OK — inherited ParentBoth(number)
cto_p.kind as "parent"; // OK
const cto_s = new cto("s"); // OK — own (string)
cto_s.kind as "child-s"; // OK
const cto_b = new cto(true); // OK — own (boolean)
cto_b.kind as "child-b"; // OK
new cto({}); // ERROR: no overload matches object

// Interface that extends multiple parents, each contributing a construct sig.
// Sigs are merged across base types as overloads — `new D(...)` resolves
// against either signature. See the NOTE on `OverloadedParams` above re
// `ConstructorParameters` and last-overload inference; same caveat applies.
interface CtorNum { new(x: number): {a: number} }
interface CtorStr { new(x: string): {b: string} }
interface CtorBoth extends CtorNum, CtorStr {}
declare const cb: CtorBoth;
const cbNum = new cb(1);
cbNum.a as number; // OK — matches CtorNum's overload
const cbStr = new cb("s");
cbStr.b as string; // OK — matches CtorStr's overload
type CtorBothParams = ConstructorParameters<CtorBoth>;
const cbp1: CtorBothParams = [1]; // OK in Flow (first overload); TS would error
const cbp2: CtorBothParams = ["s"]; // ERROR in Flow (first overload); TS would accept

// Generic class — ConstructorParameters / InstanceType must carry the
// class's tparams through find_ctor's PolyT branch. With no inference
// context, the class's `T` resolves to its top bound (`unknown`).
class GenBase<T> {
    x: T;
    constructor(x: T) { this.x = x; }
}
type GenBaseParams = ConstructorParameters<typeof GenBase>;
const gbp_num: GenBaseParams = [1]; // OK — T resolves to unknown
const gbp_str: GenBaseParams = ["s"]; // OK — same
type GenBaseFirst = GenBaseParams[0];
declare const gbf: GenBaseFirst;
gbf as unknown; // OK — GenBaseParams[0] = unknown

// Generic class instantiated and named in *type* position:
// `Class<GenBase<number>>` lowers to `ClassT(TypeAppT(GenBase, [number]))`.
// Unlike a bare `Class<C>` (an [AnnotT]-wrapped instance), the [TypeAppT] must
// be *specialized* (T := number), not just unwrapped, before the constructor
// can be read. `find_ctor` does this via the `specialize_typeapp` it is given.
type GenInstParams = ConstructorParameters<Class<GenBase<number>>>;
const gip1: GenInstParams = [1]; // OK — T = number
const gip2: GenInstParams = ["s"]; // ERROR: string incompatible with number
type GenInstInstance = InstanceType<Class<GenBase<number>>>;
declare const gii: GenInstInstance;
gii.x as number; // OK — T = number
gii.x as string; // ERROR: number incompatible with string

// Same, but through a type parameter bounded by the instantiation: `Class<X>`
// for `X: GenBase<number>` lowers to a [TypeAppT] buried under [AnnotT] /
// [GenericT], not at the top of `this`. `find_ctor` reaches it by descending
// those wrappers and specializes it there (T := number). The instance type of
// `Class<X>` is `X` itself.
function genBound<X: GenBase<number>>(): void {
  type BoundParams = ConstructorParameters<Class<X>>;
  const bp1: BoundParams = [1]; // OK — T = number
  const bp2: BoundParams = ["s"]; // ERROR: string incompatible with number
  type BoundInstance = InstanceType<Class<X>>;
  declare const bi: BoundInstance;
  bi.x as number; // OK — T = number
  bi.x as string; // ERROR: number incompatible with string
}

// Object literal flowing to construct-sig interface.
// There's exactly one focused error per missing slot.
({foo: 1}) as interface { new(): number; foo: number }; // ERROR: missing construct sig (no extra "Cannot use new on" noise)

// Interface that has BOTH a call signature and a construct signature — the
// canonical "function constructor" shape (e.g. `interface DateConstructor`).
// Exercises that the two slots [inst_call_t] and [inst_construct_t] coexist:
// `f()` dispatches the call sig, `new f()` dispatches the construct sig.
interface CtorFn {
    (x: number): {called: number};
    new(x: string): {constructed: string};
}
declare const cfn: CtorFn;
const cfnCalled = cfn(1);
cfnCalled.called as number; // OK
const cfnNew = new cfn("s");
cfnNew.constructed as string; // OK
cfn("bad"); // ERROR: call sig wants number
new cfn(1); // ERROR: construct sig wants string

// `static new(): T` on a `declare class` is an ORDINARY static method named
// "new" — not a construct sig (those live on the instance side only).
// So `K.new()` is a method call. `new K()` still works against the class's
// default constructor (which takes no args).
declare class HasStaticNew {
    static new(): {tag: "fromStatic"};
}
const fromStatic = HasStaticNew.new(); // OK — ordinary static method
fromStatic.tag as "fromStatic"; // OK — exact literal type
new HasStaticNew(); // OK — default no-arg constructor on the class itself
new HasStaticNew(1); // ERROR: default ctor takes no args (proves `static new` isn't the construct sig)

// Class<Foo> structurally satisfies interface { new(...): Foo }.
// This is the bridge enabled by storing the construct sig in [inst_construct_t]
// and comparing class `proto_props["constructor"]` against it in
// [inst_structural_subtype]. Before this commit, the assignment would fail
// because the class's statics had no `"new"` property to match.
class SubjAA {
    x: number;
    constructor(x: number) { this.x = x; }
}
type SubjAA_Ctor = interface { new(x: number): SubjAA };
const SubjAA_via_iface: SubjAA_Ctor = SubjAA; // OK — Class<SubjAA> <: interface{new(x:number): SubjAA}
const SubjAA_via_iface_bad: interface { new(x: string): SubjAA } = SubjAA; // ERROR: ctor wants number, not string
new SubjAA_via_iface(7).x as number; // OK — going through the iface, we get a SubjAA back

// `Class<C>` (the type-level form) must behave like `typeof C` for
// ConstructorParameters / InstanceType. `typeof C` reads the class *value*
// (a bare `ClassT(InstanceT)`), but `Class<C>` names the class in *type*
// position, so the instance is reached through an [AnnotT] wrapper
// (`ClassT(AnnotT(InstanceT))`). [find_ctor] must unwrap that [AnnotT] —
// otherwise the construct sig is dropped and the constraint check reports
// "construct signature missing in statics of C".
type FooClassParams = ConstructorParameters<Class<Foo>>;
const fcp1: FooClassParams = [1, "s"]; // OK
const fcp2: FooClassParams = [1, 2]; // ERROR: number incompatible with string
type FooClassInstance = InstanceType<Class<Foo>>;
declare const fci: FooClassInstance;
fci.x as number; // OK
fci as Foo; // OK

// Same through a type alias (an extra [AnnotT] layer) — still unwrapped.
type FooAlias = Foo;
type FooAliasParams = ConstructorParameters<Class<FooAlias>>;
const fap1: FooAliasParams = [1, "s"]; // OK
const fap2: FooAliasParams = ["bad", "s"]; // ERROR: string incompatible with number

// declare-class + interface declaration-merge with `new()` is exercised by
// the type-sig pipeline; see `declare_class_interface_merging_new` in
// `src/parser_utils/type_sig/__tests__/type_sig_tests.ml`. Within a single
// file, `declare class` / `interface` are handled inline by
// `type_annotation.ml` (which doesn't perform the merge), so a regression
// test for the merge itself can't live in this single-file harness.
