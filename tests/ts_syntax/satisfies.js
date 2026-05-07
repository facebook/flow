type T = unknown;
declare const m: unknown;

export const a = m satisfies T; // ERROR
