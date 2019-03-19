//@flow

declare var required_foo: $Required<
  { type: "a", a?: number } | { type: "b", b?: string }
>;

switch (required_foo.type) {
  case "a":
    (required_foo.a: number);
    break;
  case "b":
    (required_foo.b: string);
    break;
  default:
    (required_foo.type: empty);
    break;
}
