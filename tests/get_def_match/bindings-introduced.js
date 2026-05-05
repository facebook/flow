declare const x: unknown;

const e = match (x) {
  1 as foo => foo,
//            ^
  2 as const foo => foo,
//                  ^
  {const foo} => foo,
//               ^
  {...const foo} => foo,
//                  ^
  [...const foo] => foo,
//                  ^
  const foo => foo,
//             ^
};
