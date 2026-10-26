export interface S {
  s: number;
}
export as namespace SameFileA;
export as namespace SameFileB;
export as namespace SameFileA; // ERROR
