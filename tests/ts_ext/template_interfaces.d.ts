import type { Palette } from "./template_palette";

// Imported finite template keys must allow unrelated properties and retain their value types.
export interface Good extends Palette { size: number } // OK
export interface Bad extends Palette { blue1: number } // ERROR
