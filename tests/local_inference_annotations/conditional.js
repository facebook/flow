// @flow

declare var f: (x: string) => void;

f = true ? ((x) => {}) : ((y) => {});
f = true ? ((x: string | number) => {}) : ((y) => {}); // NOTE: `string => void` should be preferred as hint

const g = true ? ((x: number) => {}) : ((y) => {}); // NOTE: `number => void` should be used as hint
