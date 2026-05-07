//@flow
declare const x: {x?: {[string]: number}, ...};

const z = {...{foo: 3}, ...x.x};
