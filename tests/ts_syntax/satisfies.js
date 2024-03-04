type T = mixed;
declare var m: mixed;

export const a = m satisfies T; // ERROR
