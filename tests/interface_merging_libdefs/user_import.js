import type {
  UserMerged,
  UserReversed,
  UserContravariant,
  UserClass,
  UserClassReversed,
} from './user_export';

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
