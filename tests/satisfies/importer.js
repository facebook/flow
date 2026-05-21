import {palette, keyedNumbers} from "./exporter";

// `as const` literal types survive the export/import boundary.
palette.red as "#ff0000"; // OK
palette.green as "#00ff00"; // OK
palette.blue as "#0000ff"; // OK

// Inferred type kept the known keys, so an unknown key still errors
// after importing.
keyedNumbers.x as number; // OK
keyedNumbers.invalid; // ERROR: property `invalid` is missing
