/**
 * @flow
 */

var EventEmitter = require('events').EventEmitter;

// Bad is deferred on decl merge
var Bad = Object.assign(({}: {foo?: () => string}), EventEmitter.prototype, {
  foo: function(): string { return 'hi'; }
});

if (Bad.foo != null) {
  // Calling Bad.foo() adds `Bad` as `this`-type of `foo`
  var bad: number = Bad.foo();
}

// Good is not deferred, as MyEventEmitter is local
class MyEventEmitter extends events$EventEmitter {}
var Good = Object.assign(({}: {foo: () => string}), MyEventEmitter.prototype, {
  foo: function(): string { return 'hi'; }
});

if (Good.foo != null) {
  // Calling Good.foo() in the same file doesn't error
  var good: number = Good.foo();

  var A = {
    Bad: Bad, // assert_ground doesn't scrub out `this` type of `foo`
    Good: Good, // assert_ground scrubs out `this` type of `foo`
  };

  var good_: number = A.Good.foo(); // string ~> number

  var f = A.Bad.foo; // Property access is fine
  var bad_: number = f(); // error: string ~> number

  var bad__: number = A.Bad.foo(); // error: string, number (but `this` types are compatible)
}
