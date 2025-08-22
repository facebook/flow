// $FlowFixMe[incompatible-type]
const a: string = 1; // suppressed
// $FlowExpectedError[incompatible-type]
const b: string = 1; // suppressed
// $FlowIgnore[incompatible-type]
const c: string = 1; // not suppressed
// $FlowIssue[incompatible-type]
const d: string = 1; // not suppressed
