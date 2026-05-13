// A .ts-defined generic with a read-write Neutral tparam slot. The .ts
// gates apply to the *consuming* file's context (Files.has_ts_ext on
// Context.file cx, which is the file being checked), not to where the
// generic was declared. So this same Box should behave permissively when
// consumed from .ts and strictly when consumed from .js.

export class Animal {}
export class Dog extends Animal {}

export declare class Box<T> {
  value: T;
}
