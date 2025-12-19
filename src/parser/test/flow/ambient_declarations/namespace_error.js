// Test that const without annotation errors in declare namespace
declare namespace ErrorNamespace {
  const noAnnotation; // Error: Const must be initialized
}
