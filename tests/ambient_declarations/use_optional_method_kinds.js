import {OptionalImplicitReturn, OptionalTypeGuard} from './optional_method_kinds';

const a = new OptionalImplicitReturn();
// Implicit-any return type: callable, returns any.
const r1 = a.m?.(42); // OK

const b = new OptionalTypeGuard();
if (b.isFoo?.(null)) {
  const _: OptionalTypeGuard = b; // OK after type guard refinement
}
