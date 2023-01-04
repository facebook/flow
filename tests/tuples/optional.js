// @flow

([0, undefined]: [number, ?string]); // Ok, correct arity
([0]: [number, ?string]); // Error, arity is enforced

([]: [?number, string]); // error, since second element is not marked optional

type A = [foo: number, bar?: number]; // ERROR: syntax not supported
([1, true]: A); // OK - second element is `any` due to error
