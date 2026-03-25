import {ExportedConstants} from "./property_value_initializer_exported";

declare const e: ExportedConstants;
e.code as "EXPORTED_VALUE"; // OK
e.code as "wrong"; // ERROR - incompatible literal types
ExportedConstants.BUFFER_SIZE as 256; // OK
ExportedConstants.BUFFER_SIZE as 0; // ERROR - incompatible literal types
e.neg as -1; // OK
e.neg as 1; // ERROR
