/**
 * We also trap the class name PromisePolyfill and set its
 * type parameter to be covariant (see tests/type_param_variance
 * for the Promise case).
 *
 * Here the definition of Promise<T> is routed to PromisePolyfill
 * in libs/Promise.js, via
 * * the inclusion of libs/ in the [libs] section of .flowconfig
 * * the require('Promise') below.
 *
 * Note that in such situations we must also desugar async/await
 * to the shadowed definition.
 *
 * @flow
 */

var Promise = require('Promise');

async function foo(x: boolean): Promise<?{bar: string}> {
  if (x) {
    return {bar: 'baz'};  // OK, because of covariant type param
  } else {
    return null;
  }
}

async function run() {
  console.log(await foo(true));
  console.log(await foo(false));
}

run()
