interface UserMerged<T> {
  set(x: T): void;
}
interface UserMerged<out T> {
  get(): T;
}

interface UserReversed<out T> {
  get(): T;
}
interface UserReversed<U> {
  set(x: U): void; // ERROR
}

interface UserContravariant<T> {
  get(): T;
}
interface UserContravariant<in U> {
  set(x: U): void;
}

declare class UserClass<T> {
  readonly value: T;
}
interface UserClass<out U> {
  get(): U;
}

interface UserClassReversed<out T> {
  get(): T;
}
declare class UserClassReversed<U> {
  readonly value: U;
}

declare const userMerged: UserMerged<string>;
userMerged as UserMerged<string | number>; // ok: covariant
declare const userReversed: UserReversed<string>;
userReversed as UserReversed<string | number>; // ok: covariant
declare const userContravariant: UserContravariant<string | number>;
userContravariant as UserContravariant<string>; // ok: contravariant
declare const userClass: UserClass<string>;
userClass as UserClass<string | number>; // ok: covariant
declare const userClassReversed: UserClassReversed<string>;
userClassReversed as UserClassReversed<string | number>; // ok: covariant

userMerged as UserMerged<number>; // ERROR
userContravariant as UserContravariant<unknown>; // ERROR

export type {
  UserMerged,
  UserReversed,
  UserContravariant,
  UserClass,
  UserClassReversed,
};
