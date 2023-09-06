// @flow

export default component Foo({
//                          ^
  prop,
}: {prop: number}): ParseError {
  //...
}

export default component Bar({
//                           ^
  prop,
}: {prop: number}): ParseError {
  //...
}
