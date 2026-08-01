// @flow

// Substituting a default that references an earlier sibling must not be captured
// by a nested binder that reuses the same name (type_subst name capture). Here
// `B`'s default is the outer `A`; the inner `<A>` binder must not capture it, so
// `Capture<number>` is `<A>(x: A) => number`, not `<A>(x: A) => A`.

type Capture<A, B = A> = <A>(x: A) => B; // OK: inner `<A>` does not capture the default

declare var capture: Capture<number>; // OK
capture('') as number; // OK: B is the outer A = number
capture('') as string; // ERROR: B is number, not string
