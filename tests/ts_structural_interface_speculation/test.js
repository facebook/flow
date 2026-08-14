const interfaceMissing: RequiredInterface = resolvedLower; // ERROR: one error for the resolved structural interface mismatch
const interfaceIncompatible: RequiredInterface = incompatibleLower; // ERROR: one error for the first resolved property incompatibility
const interfaceCompatible: RequiredInterface = compatibleLower; // OK
