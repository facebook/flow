// @noflow

// annotations for disjoint unions

type T =
  | { type: "FOO", x: number }
  | { type: "BAR", x: string }

{ type: bar() as "BAR", x: str() } as T;

{ type: bar(), x: str() } as T;

{ type: bar(), x: str() as string } as T;

function bar() {
  return "BAR";
}

function str() {
  return "hello";
}
