// Computed property with identifier key
type T1 = {[MY_KEY]: number};

// Computed property with number literal key
type T2 = {[42]: number};

// Computed property with string literal key
type T3 = {["hello"]: string};

// Computed property with member expression (Symbol.iterator)
type T4 = {[Symbol.iterator]: string};

// Computed property in interface
interface I1 {
  [MY_KEY]: number;
}

// Computed property in declare class
declare class C1 {
  [MY_KEY]: number;
}

// Computed property method
type T5 = {[MY_KEY](): void};

// Computed property with optional method
type T6 = {[MY_KEY]?(x: number): void};

// Computed property with optional value
type T7 = {[MY_KEY]?: number};

// Static computed property in declare class
declare class C2 {
  static [MY_KEY]: number;
}

// Readonly computed property in declare class
declare class C3 {
  readonly [MY_KEY]: number;
}

// Multiple computed properties and indexers together
type T8 = {
  [MY_KEY]: number;
  [key: string]: string;
  [Symbol.iterator]: boolean;
};

// Accessibility modifiers with computed property in declare class
declare class C4 {
  protected [MY_KEY]: string;
  private [MY_KEY]: string;
  public [MY_KEY]: string;
}

// Indexer with label (still an indexer in .d.ts)
type T9 = {[key: string]: number};
