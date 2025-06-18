// @flow

export type Expected = {
    foo?: string,
    bar: 'foo' | 'bar',
}

declare export function assertExpected(expected: Expected): void;
