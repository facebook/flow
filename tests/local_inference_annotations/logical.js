// @flow

declare var f: (x: string) => void;

f = (x => {}) || (y => {});
f = (x => {}) || (y => {}) || (z => {});
f = ((x: string | number) => {}) || (y => {}); // NOTE: `string => void` should be preferred as hint

const or = ((x: number) => {}) || (y => {}); // NOTE: `number => void` should be used as hint

f = (x => {}) && (y => {});
f = (x => {}) && (y => {}) && (z => {});
f = ((x: string | number) => {}) && (y => {}); // NOTE: `string => void` should be preferred as hint

const and = ((x: number) => {}) && (y => {}); // Error `y` missing annotation

f = (x => {}) ?? (y => {});
f = (x => {}) ?? (y => {}) ?? (z => {});
f = ((x: string | number) => {}) ?? (y => {}); // NOTE: `string => void` should be preferred as hint

const nco = ((x: number) => {}) ?? (y => {}); // NOTE: `number => void` should be used as hint
