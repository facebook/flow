/* @flow */

function makeIterator1(coin_flip: () => boolean ): Iterator<string> {
  return {
    "@@iterator"() { return makeIterator1(coin_flip); },
    next(): IteratorResult<string, void> {
      var done = coin_flip();
      if (!done) {
        return { done, value: "still going..." };
      } else {
        return { done };
      }
    }
  }
}

function makeIterator2(coin_flip: () => boolean ): Iterator<string> {
  return {
    "@@iterator"() { return makeIterator2(coin_flip); },
    next(): IteratorResult<string, void> {
      var done = coin_flip();
      if (done) { // Whoops, made a mistake and forgot to negate done
        return { done, value: "still going..." }; // Error string ~> void
      } else {
        return { done }; // Error void ~> string
      }
    }
  }
}
