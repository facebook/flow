1 as string; // not suppressed
// $FlowFixMe[incompatible-cast]
1 as string; // not suppressed
// $FlowFixMe[incompatible-type]
1 as string; // suppressed

declare function expectString(v: string): void;
expectString(1); // not suppressed
// $FlowFixMe[incompatible-call]
expectString(1); // not suppressed
// $FlowFixMe[incompatible-type]
expectString(1); // suppressed

function testReturn1(): string { return 1 } // not suppressed
// $FlowFixMe[incompatible-return]
function testReturn2(): string { return 1 } // not suppressed
// $FlowFixMe[incompatible-type]
function testReturn3(): string { return 1 } // suppressed

const _testProp1: {prop: string} = {}; // not suppressed
// $FlowFixMe[prop-missing]
const _testProp2: {prop: string} = {}; // not suppressed
// $FlowFixMe[incompatible-type]
const _testProp3: {prop: string} = {}; // suppressed

declare opaque type ExpectString<T: string>;
type T1 = ExpectString<1>; // not suppressed
// $FlowFixMe[incompatible-type-arg]
type T2 = ExpectString<1>; // not suppressed
// $FlowFixMe[incompatible-type]
type T3 = ExpectString<1>; // suppressed

declare class Base { foo: string }
class TestExtend1 extends Base { foo: 1 } // not suppressed
// $FlowFixMe[incompatible-extend]
class TestExtend2 extends Base { foo: 1 } // not suppressed
// $FlowFixMe[incompatible-type]
class TestExtend3 extends Base { foo: 1 } // suppressed
