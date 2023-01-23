// @flow

declare var arr: Array<{ +f: ?number }>;
let acc = [];
arr.forEach(x => acc.push(x.f));
// $FlowExpectedError in env-mode ssa
acc = acc.filter(Boolean);
