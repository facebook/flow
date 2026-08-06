// The imported shapes must behave exactly as the within-file ones do, so the
// signature pipeline and the checking pipeline agree on which keys are named
// properties and which are index signatures.
import {
  DcMethodThenValue,
  DcStaticOverloads,
  DcValueThenMethod,
  Holder,
  MergedHolder,
} from './exp';
import type {
  One,
  Multi,
  Num,
  Mixed,
  Opt,
  Labeled,
  FromConst,
  FromQualified,
  FromWide,
  OneAlias,
  PlainWins,
  LiteralWins,
  ValueThenPlain,
  PlainThenValue,
  ValueThenMethod,
  MethodThenValue,
  Overloads,
  AccessorFirst,
  AccessorLast,
  AccessorMiddle,
  AccessorSplit,
  Dual,
  FromImportedConst,
  ReadOnlyKey,
  FromClass,
} from './exp';

declare const one: One;
one.a as number; // OK
one.a as string; // ERROR: number is not string
one.b; // ERROR: property `b` is missing (a named key, not an indexer)
const emptyOne: One = {}; // ERROR: a named property is required

declare const multi: Multi;
multi.a as number; // OK
multi.b as string; // OK

declare const num: Num;
num[42] as boolean; // OK
num[99]; // ERROR: property `99` is missing (a named key, not a number indexer)

declare const mixed: Mixed;
mixed.bar as boolean; // OK, named from the literal key
mixed.bar as number; // ERROR: boolean is not number, so this is not the indexer
mixed['other'] as number; // OK via the index signature
mixed.other; // ERROR: an interface index signature answers no named read

declare const opt: Opt;
opt.a as number | void; // OK
opt.a as number; // ERROR: the property is optional, so `void` is not a number

declare const labeled: Labeled;
labeled['a'] as number; // OK via the indexer
labeled.a; // ERROR: unlike the literal key above, this key names no property
const emptyLabeled: Labeled = {}; // OK, an index signature requires nothing

declare const fromConst: FromConst;
fromConst.foo as number; // OK
fromConst.foo as string; // ERROR: number is not string

declare const fromQualified: FromQualified;
fromQualified.a as number; // OK
fromQualified.foo; // ERROR: the key is the value `'a'`, not the name `foo`

declare const fromWide: FromWide;
fromWide.anything; // ERROR: the member was dropped, so nothing is there

declare const oneAlias: OneAlias;
oneAlias['a'] as number; // OK via the indexer
const emptyAlias: OneAlias = {}; // OK, an index signature requires nothing

declare const plainWins: PlainWins;
plainWins.a as string; // OK, the later plain property wins
plainWins.a as number; // ERROR: string is not number

declare const literalWins: LiteralWins;
literalWins.a as number; // OK, the later literal-key property wins
literalWins.a as string; // ERROR: number is not string

declare const valueThenPlain: ValueThenPlain;
valueThenPlain.a as string; // OK, the later plain property wins
valueThenPlain.a as number; // ERROR: string is not number

declare const plainThenValue: PlainThenValue;
plainThenValue.a as number; // OK, the later value key wins
plainThenValue.a as string; // ERROR: number is not string

declare const valueThenMethod: ValueThenMethod;
valueThenMethod.a() as void; // OK, the later method wins
valueThenMethod.a as number; // ERROR: the method is not a number

declare const methodThenValue: MethodThenValue;
methodThenValue.a as number; // OK, the later value key wins
methodThenValue.a(); // ERROR: a number is not callable

declare const overloads: Overloads;
overloads.a(1) as number; // OK, the first overload survived the key
overloads.a('s') as string; // OK, and so did the second
overloads.a(true); // ERROR: still a method, so a boolean matches no overload

new DcValueThenMethod().a as number; // OK, the field shadows the method
new DcMethodThenValue().a as number; // OK, and in the other order too
DcStaticOverloads.a(1) as number; // OK, the key lost to the overloads
DcStaticOverloads.a('s') as string; // OK, both of them survived

declare const mergedHolder: MergedHolder;
mergedHolder.a as string; // OK, the class field wins over the merged key
mergedHolder.a as number; // ERROR: string is not number

declare const accessorFirst: AccessorFirst;
accessorFirst.p as boolean; // OK, the key follows both halves
accessorFirst.p as number; // ERROR: boolean is not number

declare const accessorLast: AccessorLast;
accessorLast.p as number; // OK, both halves follow the key
accessorLast.p as boolean; // ERROR: number is not boolean

declare const accessorMiddle: AccessorMiddle;
accessorMiddle.p as number; // OK, the pair wins whole
accessorMiddle.p as boolean; // ERROR: number is not boolean

declare const accessorSplit: AccessorSplit;
accessorSplit.a; // ERROR: property `a` is not readable
accessorSplit.a = 1; // OK, the literal key split the pair, leaving the setter

declare const dual: Dual;
dual['b'] as number; // OK via the indexer over 'b'
dual['a']; // ERROR: 'a' is the value, and the type won

declare const fromImportedConst: FromImportedConst;
fromImportedConst.shared as number; // OK, named from the imported constant
fromImportedConst.KEY; // ERROR: the key is the value, not the binding's name

declare const readOnlyKey: ReadOnlyKey;
readOnlyKey.a as number; // OK
readOnlyKey.shared as string; // OK
readOnlyKey.a = 1; // ERROR: property `a` is not writable

declare const fromClass: FromClass;
fromClass.anything; // ERROR: the member was dropped, so nothing is there

declare const holder: Holder;
holder.a as number; // OK
holder.a as string; // ERROR: number is not string
holder.dk as number; // OK
Holder.b as string; // OK
Holder.dk as string; // OK
Holder.a; // ERROR: `a` is an instance property, not a static one
holder['other'] as boolean; // OK via the index signature
