// Even if `static abstract` syntax becomes reachable (today the parser
// blocks it with ParseError), the type-sig pipeline must NOT thread the
// static abstract name into instance-side inst_abstract_props. This test
// covers the instance-side path (which IS reachable today) — the static
// filter is defensive against future parser changes that enable
// `static abstract`.

declare abstract class D {
  abstract instanceM(): void;
}

class E extends D { // OK: implements the instance abstract member
  instanceM(): void {}
}

class F extends D {} // ERROR: missing instanceM
