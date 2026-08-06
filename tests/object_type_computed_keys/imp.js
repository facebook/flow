// The imported types must behave exactly as the within-file ones do, so the
// signature pipeline and the checking pipeline agree on which keys are named
// properties and which are index signatures.
import type {
  One,
  Multi,
  Num,
  Dict,
  Union,
  Mixed,
  Opt,
  Proto,
  FromConst,
  FromQualified,
  FromWide,
  OneAlias,
  KeyofOne,
  LiteralBound,
  ValueParam,
  LabeledLit,
  PlainWins,
  LiteralWins,
  ValueThenPlain,
  PlainThenValue,
  Dual,
  AccessorFirst,
  AccessorLast,
  AccessorMiddle,
  FromImportedConst,
  NegKey,
  ReadOnlyKey,
  WriteOnlyKey,
  SpreadOverridesLiteral,
  LiteralOverridesSpread,
  FromClass,
  UnlabeledMerged,
  LabeledMerged,
} from './exp';

declare const one: One;
one.a as number; // OK
one.a as string; // ERROR: number is not string
one.b; // ERROR: property `b` is missing (exact, no indexer default)
const emptyOne: One = {}; // ERROR: a named property is required, unlike an indexer

declare const multi: Multi;
multi.a as number; // OK
multi.b as string; // OK

declare const num: Num;
num[42] as boolean; // OK
num[99]; // ERROR: property `99` is missing (named key, not a number indexer)

declare const dict: Dict;
dict.anything as number; // OK via the indexer

declare const u: Union;
u.a as number; // OK via the indexer
u.c; // ERROR: `c` is not in the 'a' | 'b' indexer key set

declare const mixed: Mixed;
mixed.foo as string; // OK
mixed.bar as boolean; // OK, named from the literal key
mixed.bar as number; // ERROR: boolean is not number, so this is not the indexer
mixed.other as number; // OK via the index signature

declare const opt: Opt;
opt.a as number | void; // OK, optional named property
opt.a as number; // ERROR: property is optional, so `void` is not a number

declare const p: Proto;
p.__proto__ as number; // OK, own property from the `['__proto__']` literal key
p.__proto__ as string; // ERROR: number is not string

declare const fromConst: FromConst;
fromConst.foo as number; // OK, the value key names property `foo`
fromConst.bar; // ERROR: property `bar` is missing (named, not an indexer)

declare const fromQualified: FromQualified;
fromQualified.a as number; // OK, the qualified value key names property `a`
fromQualified.foo; // ERROR: the key is the value `'a'`, not the name `foo`

declare const fromWide: FromWide;
fromWide.anything; // ERROR: the member was dropped, as it is within the file
const emptyWide: FromWide = {}; // OK, the member was dropped

declare const oneAlias: OneAlias;
oneAlias.a as number; // OK via the indexer (a type name, not a value)
const emptyAlias: OneAlias = {}; // OK, an index signature requires nothing

declare const keyofOne: KeyofOne;
keyofOne.a as string; // OK via the indexer
keyofOne.a as number; // ERROR: string is not number
const emptyKeyofOne: KeyofOne = {}; // OK

declare const literalBound: LiteralBound<'a'>;
literalBound.a as number; // OK via the indexer
literalBound.a as string; // ERROR: number is not string

declare const valueParam: ValueParam<number>;
valueParam.a as number; // OK, named property
valueParam.b; // ERROR: property `b` is missing (named, not an indexer)

declare const labeledLit: LabeledLit;
labeledLit.a as number; // OK via the indexer (labeled, not a named property)
labeledLit.a as string; // ERROR: number is not string

declare const plainWins: PlainWins;
plainWins.a as string; // OK, later plain property wins (same as within.js)
plainWins.a as number; // ERROR: string is not number

declare const literalWins: LiteralWins;
literalWins.a as number; // OK, later literal-key property wins
literalWins.a as string; // ERROR: number is not string

declare const valueThenPlain: ValueThenPlain;
valueThenPlain.a as string; // OK, later plain property wins (same as value_keys.js)
valueThenPlain.a as number; // ERROR: string is not number

declare const plainThenValue: PlainThenValue;
plainThenValue.a as number; // OK, later value key wins
plainThenValue.a as string; // ERROR: number is not string

declare const spreadOver: SpreadOverridesLiteral;
spreadOver.a as number; // OK, `...Base` overrides the earlier literal key
spreadOver.a as string; // ERROR: number is not string

declare const litOver: LiteralOverridesSpread;
litOver.a as string; // OK, the literal key after the spread wins
litOver.a as number; // ERROR: string is not number

declare const dual: Dual;
dual.b as number; // OK via the indexer over 'b' (same as value_keys.js)
dual.a; // ERROR: 'a' is the value, and the type won

declare const accessorFirst: AccessorFirst;
accessorFirst.a as boolean; // OK, the computed key follows both accessor halves

declare const accessorLast: AccessorLast;
accessorLast.a as number; // OK, both accessor halves follow the computed key

declare const accessorMiddle: AccessorMiddle;
accessorMiddle.a as number; // OK, the pair wins whole, as it does in the file
accessorMiddle.a as boolean; // ERROR: number is not boolean

declare const fromImportedConst: FromImportedConst;
fromImportedConst.shared as number; // OK, named from the imported constant
fromImportedConst.KEY; // ERROR: the key is the value, not the binding's name

declare const negKey: NegKey;
negKey[-1] as boolean; // OK
negKey[1]; // ERROR: property `1` is missing

declare const readOnlyKey: ReadOnlyKey;
readOnlyKey.a as number; // OK
readOnlyKey.shared as string; // OK
readOnlyKey.a = 1; // ERROR: property `a` is not writable
readOnlyKey.shared = 'x'; // ERROR: property `shared` is not writable either

declare const writeOnlyKey: WriteOnlyKey;
writeOnlyKey.a as number; // ERROR: property `a` is not readable
writeOnlyKey.a = 1; // OK

declare const fromClass: FromClass;
fromClass.anything; // ERROR: the member was dropped, as it is within the file
const emptyFromClass: FromClass = {}; // OK, the member was dropped

declare const unlabeledMerged: UnlabeledMerged;
unlabeledMerged.merged; // ERROR: the rejected member was dropped by the signature
const emptyUnlabeledMerged: UnlabeledMerged = {}; // OK, the member was dropped

declare const labeledMerged: LabeledMerged;
labeledMerged.merged as number; // OK via the imported indexer
labeledMerged.other; // ERROR: `other` is not in the 'merged' indexer key set
const emptyLabeledMerged: LabeledMerged = {}; // OK, an index signature requires nothing
