//@flow

function test(x: React.ComponentType<any>) {
  if (x.displayName) {} // We can look for props on components
  if (x.notOnEitherSFCOrClass) {} // Error Not on any component
}
