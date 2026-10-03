declare function assertNumber(value: unknown): asserts value is number; // error: unsupported syntax

function assertTruthy(value: unknown): asserts value {} // error: unsupported syntax
