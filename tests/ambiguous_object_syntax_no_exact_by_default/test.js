//@flow
const w: {[string]: number} = {};
const x: {} = {}; // lint
const y: {...} = {}; // Ok
const z: {||} = {}; // Error
