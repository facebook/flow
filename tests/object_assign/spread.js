declare var arrOfObjs: Array<{ foo: string }>;
declare var roArrOfObjs: ReadonlyArray<{foo: string}>;
declare var tup: [{foo: string}, {bar: number}];

Object.assign({} as {foo?: string}, ...arrOfObjs) as { foo?: number}; // Error: string ~> number
Object.assign({} as {foo?: string}, ...roArrOfObjs) as { foo?: number}; // Error: string ~> number
Object.assign({} as {foo?: string, bar?: number}, ...tup) as { foo?: string, bar?: boolean}; // Error: number ~> boolean

(Object.assign(
  {} as {a?: number, b?: string, c?: boolean},
  ...[{a: 1}, {b: 'foo'}],
  ...[{c: true}],
) as {a?: number, b?: true, c?: boolean}); // Error: 'foo' => true
