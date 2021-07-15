// @flow

const Module = require("module");

(Module.Module.Module.Module.Module: typeof Module);
(Module.builtinModules.includes("module"): boolean);
const buffer = Module.createRequire("/dev/null")("buffer");
