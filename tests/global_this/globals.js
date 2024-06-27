interface Window {
  myWeirdWindowScopedGlobal: string;
}
declare const window: Window & typeof globalThis;

declare opaque type Opaque;
declare const myGlobal: Opaque;
