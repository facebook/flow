//@flow

/**
 * first declaration
 */
declare function redeclare(x: number): number;

/**
 * second declaration
 */
declare function redeclare(x: string): string;

/**
 * export declaration
 */
export default function redeclare(x) { return x; };
