declare var arr: Array<{ +f: ?number }>;
let acc = [];
arr.forEach(x => acc.push(x.f));
// $FlowExpectedError[incompatible-type]
acc = acc.filter(Boolean);
