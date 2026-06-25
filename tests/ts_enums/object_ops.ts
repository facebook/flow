// The enum value is usable as an object: Object.keys/values/entries, spread, and
// passing it where an object is expected all work (it forwards to its members).
enum Color {
  Red = 1,
  Green = 2,
}

const keys = Object.keys(Color); // OK
keys[0] satisfies string; // OK: keys are the member names ("Red" | "Green")
Object.values(Color); // OK
Object.entries(Color); // OK

const spread = { ...Color }; // OK
spread.Red satisfies number; // OK: 1

declare function takesObj(o: { readonly [string]: unknown }): void;
takesObj(Color); // OK
