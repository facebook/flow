// The parser pre-empts the typechecker here with a [ParseError]
// ("Abstract methods can only appear within an abstract class.").
// FIXME[invalid-abstract]: when the parser is relaxed under the flag,
// the typechecker should emit [invalid-abstract]
// (AbstractMemberOnNonAbstractClass) for `abstract` members declared in
// a non-`abstract` class.
class NotAbs {
  abstract m(): void; // ERROR
}
