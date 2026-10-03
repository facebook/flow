declare function myAssert(condition: boolean): asserts condition;

// Assertion functions only affect control flow when called as expression
// statements. In expression position these calls return `void`.
function reproNeverReturnsInMatch(type: string): string {
  return match (type) {
    'A' => 'a',
    'B' => 'b',
    _ => myAssert(false),
  };
}

function reproNeverReturnsInReturn(): empty {
  return myAssert(false);
}

function reproNeverReturnsInNestedTernary(level: string): string {
  const s =
    level === 'ad_set' ? 'a' : level === 'campaign' ? 'b' : myAssert(false);
  return s;
}

function sequenceExpressionAsserts(value: ?number): number {
  (myAssert(value != null), value as number);
  return value;
}
