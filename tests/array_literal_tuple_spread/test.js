function withoutAny(tup: [1,2], notAny: [3, 4]): [1, 2, 3, 4] {
   return [...tup, ...notAny]; // ok
}
// any flowing to spreads
function withAny(tup: [1,2], any: any): [1, 2, 3, 4] {
   return [...tup, ...any]; // ok
}

/* We used to try to "summarize" elements of non-tuple arrays, which would
 * strip away literal information from string and number types. However, this
 * was pretty broken, and Sam found this following example to demonstrate how.
 */
// Sam's example of multiple lower bounds and SummarizeT
function f(b: boolean): [Array<?number>] {
  // x has multiple lower bounds, so if we unify prematurely we can get
  // an error when figuring out the summarized element type for x
  var x = null;
  if (b) {
    x = 0;
  }
  var [xs] = f(b);
  return [[...xs, x]]; // ok
}

// Spreading in a tuple should produce another tuple
{
  const a = [2];
  const b = [4, 5];
  const x: [1,20,30,4,5,60] = [1, ...a, 3, ...b, 6]; // error
}

// Explicit union should become an explicit union
function test(arr: [1] | [2, 3]): [1, 10] | [2, 3, 10] {
  return [...arr, 10]; // ok
}

// Non-polymorphic function
{
  function foo(arr: Array<number>) {
    return [...arr, 1];
  }
  const ret1 = foo([2]);
  const ret2 = foo([3]);
  (ret1[0]: 2); // error
  (ret2[0]: 3); // error
}

// Spreading an Array<T> should result in a non-tuple array
{
  const tup: Array<number> = [1,2,3];
  const nonTup = [...tup];
  (nonTup: [1,2,3]); // error
}

// Spreading an $ReadOnlyArray<T> should result in a non-tuple array
{
  const tup: $ReadOnlyArray<number> = [1,2,3];
  const nonTup = [...tup];
  (nonTup: [1,2,3]); // error
}

// Spreading a string
([..."hello"]: Array<number>); // ERROR

// Spreading a generator
{
  function *foo(): Generator<string, void, void> {
    yield "hello";
  }
  const arr: Array<number> = [...foo()]; // error: Generators are iterables too!
}

// Spreading an iterator
{
  function test(iter: Iterable<string>): Array<number> {
    return [...iter]; // error: Spec says you can spread iterables
  }
}

// Spreading Object
{
  function test(iter: Object): string {
    return [...iter]; // ok
  }
}
