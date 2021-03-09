//@flow
const w: {[string]: number} = {};
const x: {} = {}; // Error + lint
const y: {...} = {}; // Ok
const z: {||} = {}; // Error
