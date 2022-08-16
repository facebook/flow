var x: { } = { foo: 0 };
var y: { foo?: string } = x; // OK in TypeScript, not OK in Flow

var z: string = y.foo || "";

function bar(config: { foo?: number }) {}
bar({});
bar({foo: ""});
