//@flow
const w: {[string]: number} = {};
const x: {} = {}; // Lint
const y: {...} = {}; // Ok
const z: {||} = {}; // Ok
