type A1 = renders B;
//         ^
type A2 = renders? B;
//         ^
type A3 = renders* B;
//         ^

declare component C1() renders B;
//                        ^
declare component C2() renders? B;
//                        ^
declare component C3() renders* B;
//                        ^
