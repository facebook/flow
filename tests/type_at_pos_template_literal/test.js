// Eagerly resolved: single string
type T1 = `hello`;
//   ^

// Eagerly resolved: single interpolation
type T2 = `prefix_${'a' | 'b'}`;
//   ^

// Eagerly resolved: cross product
type T3 = `${'a' | 'b'}_${'x' | 'y'}`;
//   ^

// Unresolved: string interpolation
type T4 = `prefix_${string}`;
//   ^

// Unresolved: number interpolation
type T5 = `item_${number}`;
//   ^

// Unresolved: mixed concrete and non-concrete
type T6 = `${'a' | 'b'}_${string}_suffix`;
//   ^

// Quasi containing a backtick: must be escaped to \`
type T7 = `a\`b_${string}`;
//   ^

// Quasi containing a backslash: must be escaped to \\
type T8 = `a\\b_${string}`;
//   ^

// Quasi containing a literal ${ (escaped at the source) must be re-escaped
type T9 = `a\${b_${string}`;
//   ^
