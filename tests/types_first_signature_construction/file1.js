// @flow

// The exported signature should be: (string) => string
function bar(x: string): string { return ""; }
declare function bar(x: number): number; // error: cannot declare

bar("");
bar(42);
bar(true);

module.exports = bar;
