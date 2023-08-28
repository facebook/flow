// Naive any-propagation for PromoteRendersRepresentationT would forward `any`
// instead of the original object, causing $Renders<React.Node> to be any
class Bar {}
component Foo() {
  return Bar; // ERROR
}
