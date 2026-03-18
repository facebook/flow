// Regression test for in_async_scope missing AsyncComponentOrHookBody.
// Without the fix, async hook/component bodies are not recognized as
// async scopes (in_async_scope returns false), affecting type checking
// of await expressions and promise handling in these bodies.

// The `async hook` keyword sets ScopeKind::AsyncComponentOrHookBody.
// Without the fix, in_async_scope did not check for this scope kind.

async hook useAsyncData(input: number): number {
  return input + 1;
}

async hook useChained(): string {
  return "hello";
}

// Error case: wrong return type
async hook useBadReturn(): string {
  return 42; // ERROR: number incompatible with string
}
