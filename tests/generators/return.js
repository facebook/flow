function test1(gen: Generator<void, string, void>) {
  // You can pass whatever you like to return, it doesn't need to be related to
  // the Generator's return type
  var ret = gen.return(123);
  (ret.done: false); // Error true ~> false
  (ret.value: 456); // Error 123 ~> 456
}
