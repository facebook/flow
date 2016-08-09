/**
 * subtyping defeats property existence testing, and even successful
 * refinements on inexact types may be weaker than expected due
 * to subtyping. see newtests/exact for exact type cases
 */

const tests = [

  function(x: { done: true, result: string } | { done: false }) {
    // done is a sentinel property present in all members, so testable
    if (x.done) {
      // refined to first union member
      return x.result;  // ok
    } else {
      // refined to second union member
      return x.result;  // error, consider x = { done: false, result: 0 }
    }
  },

  function(x: { done: true, result: string } | { foo: string }) {
    if (x.done) { // error: done is not declared in all members, so not testable
      // consider { foo: "herp", done: "derp" }
    }
  },
]
