//@flow

declare const obj : ?{
  foo : number,
  'foo.bar.baz' : number,
}

obj.
//  ^
