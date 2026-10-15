import Value = require("./export_assignment"); // ERROR
import type ValueType = require("./export_assignment"); // ERROR

type T = number;
export {type T}; // ERROR
