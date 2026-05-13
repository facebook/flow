// Object literal flowing into a target type whose properties are optional
// and read-write (Polarity.Neutral OptionalT). Flow's pre-gate behavior was
// to emit "These optional properties of ... are invariantly typed" and unify
// the upper property type with `any`, which forced inferred tparams to
// resolve invariantly. TS accepts the literal because the keys are optional.
// In .ts files Flow now skips the invariance demand and runs a covariant
// flow instead -- genuine type incompatibility still surfaces via
// [incompatible-type], but the polarity-only error is gone.

// 1. Canonical Partial<Record<S, T | undefined>> -- the SlotRecord shape.
type SlotRecord<S extends string, T> = Partial<Record<S, T | undefined>>;
const slot: SlotRecord<"sm" | "md" | "lg", {description: string}> = {
  sm: {description: "small"},
}; // OK in .ts: omitted "md"/"lg" no longer trigger invariance

// 2. Generic instantiation where T lands in an optional, neutral slot.
declare function take<T>(x: Partial<Record<"a" | "b", T | undefined>>): T;
const t: {description: string} = take({a: {description: "x"}}); // OK in .ts

// 3. Anonymous mapped type with optional+neutral properties. Use an explicit
// type argument so this remains a TS-compatible optional-input case while
// still requiring the returned type to include `y`.
type OptMap<O> = {[K in keyof O]?: O[K]};
declare function consume<O>(x: OptMap<O>): O;
const o: {x: number; y: number} = consume<{x: number; y: number}>({x: 1}); // OK in .ts

// 4. Negative case: the gate replaces invariance with covariance, not
// bivariance. A genuine type-arg incompatibility must still error.
declare function strict(x: Partial<Record<"a", string>>): void;
strict({a: 42}); // ERROR: number not assignable to string
