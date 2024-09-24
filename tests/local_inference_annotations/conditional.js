// @flow

declare var f: (x: string) => void;

f = true ? ((x) => {}) : ((y) => {});
f = true ? ((x: string | number) => {}) : ((y) => {}); // NOTE: `string => void` should be preferred as hint

const test1 = true ? ((x: number) => {}) : ((y) => {}); // NOTE: `number => void` should be used as hint

const test2 = true ? [] : [1];
test2 as Array<number>;

const test3 = true ? [x => {}] : [(x: number) => {}];
test3 as $ReadOnlyArray<(number) => void>;
test3 as $ReadOnlyArray<(string) => void>; // error should be number => void
