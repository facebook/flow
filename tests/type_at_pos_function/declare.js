// @flow

declare function foo(): void;

declare function bar(): string;
declare function bar(): number;

declare function baz(foo: string): boolean %checks(foo === 'bar');
declare function baz(foo: number): boolean %checks(foo === 3);
