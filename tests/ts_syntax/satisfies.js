type T = unknown;
declare var m: unknown;

export const a = m satisfies T; // ERROR
