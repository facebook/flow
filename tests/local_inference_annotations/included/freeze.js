// @flow

const noHint = Object.freeze({ f: x => {}}); // error
const withHint: { f: number => void } = Object.freeze({ f: x => {}});
(Object.freeze({ f: x => {}}): { f: number => void });
