interface LibOptions {
  name?: string;
}

// Overrides in library definitions follow TypeScript's rules.
interface LibLocale extends LibOptions {
  name: string;
}

interface LibNarrowed extends LibOptions {
  readonly name: string;
}

declare class LibBase {
  x: string | number;
}

declare class LibDerived extends LibBase {
  x: string;
}
