declare const tag: <T>(strings: TaggedTemplateLiteralArray) => T;

const x: string = tag<string>`hello`; // ok
tag<number>`hello` as empty; // ERROR
