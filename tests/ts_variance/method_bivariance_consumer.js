// @flow
// .js consumer of the same method-syntax shape that .ts accepts via
// bivariance. The gate keys on Files.has_ts_ext (Context.file cx), so a
// .js context must NOT relax method-syntax param checking -- the narrower
// param assignment that's accepted in .ts via bivariance still errors here.

declare class Animal {
  name: string;
}
declare class Dog extends Animal {
  bark(): void;
}

type MethodHolder = {
  cb(x: Animal): void,
};

// Same shape as method_bivariance.ts:23 (which is OK in .ts). In .js, the
// standard contravariant FunT~>FunT path runs and rejects this.
const m: MethodHolder = {cb(x: Dog): void {}}; // ERROR in .js: no bivariance
