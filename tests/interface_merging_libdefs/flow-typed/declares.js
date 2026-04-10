// Basic property merging
interface MergedProps {
  a: string;
}
interface MergedProps {
  b: number;
}

// Method overload merging
interface MergedMethods {
  foo(s: string): string;
}
interface MergedMethods {
  foo(n: number): number;
  bar(): boolean;
}

// Extends merging
interface Base1 { x: string; }
interface Base2 { y: number; }
interface MergedExtends extends Base1 {
  a: string;
}
interface MergedExtends extends Base2 {
  b: number;
}

// Call signature merging
interface MergedCalls {
  (): string;
}
interface MergedCalls {
  (): number;
}

// Three-way merge
interface ThreeWay {
  first: string;
}
interface ThreeWay {
  second: number;
}
interface ThreeWay {
  third: boolean;
}
