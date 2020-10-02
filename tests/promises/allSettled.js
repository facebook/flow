// @flow

// First argument is required
Promise.allSettled(); // Error: expected $Iterable instead of undefined (too few arguments)

// Invalid arg type
Promise.allSettled(0); // Error: expected $Iterable instead of number

// Promise.allSettled is a function
(Promise.allSettled : Function);

class Foo {
  foo() {}
}

class Bar {
  bar() {}
}

// Promise.allSettled() can take a heterogeneous list of Promises (or PromiseLikes)
// and Flow should be able to discern the appropriate types within the tuple that is
// returned. It should also be able to do proper exhaustiveness checking on the
// status of a $SettledPromiseResult.
async function test1(): Promise<[$SettledPromiseResult<Foo>, $SettledPromiseResult<Bar>]> {
  const foo = Promise.resolve(new Foo());
  const bar = Promise.resolve(new Bar());
  const settled = await Promise.allSettled([foo, bar]);

  const first = settled[0];
  switch (first.status) {
    case 'fulfilled': {
      console.log(first.value.foo());
      break;
    }
    default:
      (first.status: empty) // Error: 'rejected' case was not covered
      throw Error(`unknown status: ${first.status}`);
  }

  const second = settled[1];
  if (second.status === 'fulfilled') {
    console.log(second.value.foo()); // Error: second is known to be a Bar, not a Foo
  } else if (second.status === 'rejected')  {
    console.log(second.reason);
  } else {
    return (second.status: empty);
  }

  return settled;
}

// Promise.allSettled() can take a mix of Promises and non-Promises.
async function test2(): Promise<[
  $SettledPromiseResult<Foo>,
  $SettledPromiseResult<Bar>,
  $SettledPromiseResult<number>,
  $SettledPromiseResult<string>,
  $SettledPromiseResult<void>,
]> {
  return Promise.allSettled([new Foo(), new Bar(), 400, 'Bar', Promise.resolve()]);
}
