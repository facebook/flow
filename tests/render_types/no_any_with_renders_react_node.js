// Naive any-propagation for promote_renders_reprensentation would forward `any`
// instead of the original object, causing renders React.Node to be any
class Bar {}
component Foo() {
  return Bar; // ERROR
}
