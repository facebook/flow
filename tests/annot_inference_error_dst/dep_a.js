declare export const o: ?{p: number};
export type P = typeof o.p; // ERROR
declare export const y: number;
