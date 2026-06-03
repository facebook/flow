// `override abstract` (reverse order) is a parse error. The parser reads
// `abstract` first, then `override`, so a leading `override` followed by
// `abstract` is reported as an unexpected identifier.

abstract class Base {
  foo(): void {}
}

abstract class Sub extends Base {
  override abstract foo(): void; // PARSE ERROR: wrong modifier order
}
