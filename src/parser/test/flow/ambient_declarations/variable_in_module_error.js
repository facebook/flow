// Test that const without annotation errors in flow-typed declare module
declare module 'test-module-error' {
  const noAnnotation; // Error: Const must be initialized
}
