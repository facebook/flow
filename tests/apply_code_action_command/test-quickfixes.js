// $FlowFixMe[internal-type]
type T1 = React$Node;
// $FlowFixMe[internal-type]
type T2 = React$Node;

type Expected = {
    foo?: number,
    bar: 'foo' | 'bar',
}

declare export function assertExpected(expected: Expected): void;

const obj = {bar: 'foo' as const};
// $FlowFixMe[incompatible-type]
assertExpected(obj);
