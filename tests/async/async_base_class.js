// This is kind of weird, but it should parse. This works in babel without the
// parens around (await promise). From the es6 and async/await specs I (nmote)
// am not clear on whether it should. In any case it's a strange corner case
// that is probably not important to support.
async function foo() {
  class Bar extends (await promise) { }
  return Bar;
}
