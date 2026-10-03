type Asserts = (x: unknown) => asserts x is string;
type Guard = (x: unknown) => x is string;
type Implies = (x: unknown) => implies x is string;
type Plain = (x: unknown) => boolean;

declare var assertion: Asserts;
declare var guard: Guard;
declare var implies: Implies;
declare var plain: Plain;

assertion as Asserts;
assertion as Guard; // error: assertion and conditional guards are distinct
assertion as Implies; // error: assertion and one-sided guards are distinct
assertion as Plain; // error: assertions return void
guard as Asserts; // error: conditional guard is not an assertion
implies as Asserts; // error: one-sided guard is not an assertion
plain as Asserts; // error: plain function is not an assertion

type BareAsserts = (x: unknown) => asserts x;
declare var bare: BareAsserts;

bare as BareAsserts;
bare as Asserts; // error: truthiness assertion and typed assertion differ
assertion as BareAsserts; // error: typed assertion and truthiness assertion differ

type AssertsSecond = (x: unknown, y: unknown) => asserts y is string;
type AssertsFirst = (x: unknown, y: unknown) => asserts x is string;
declare var second: AssertsSecond;

second as AssertsFirst; // error: asserted parameter positions differ

