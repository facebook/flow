// Ambient TS exports must preserve finite template keys and selector patterns.
export type Names = { blue: string; red: string };
export type Keys = `${keyof Names}${1 | 2}`;
export type Palette = { [K in `${keyof Names}${1 | 2}`]: string };
export type AttributeSelector = `&[${string}]`;
export type Selector = AttributeSelector | `${string} &`;

// Same-file inference already restricts the mapped type to its finite keys.
export interface LocalGood extends Palette { size: number } // OK
export interface LocalBad extends Palette { blue1: number } // ERROR
