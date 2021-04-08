//@flow

declare var obj : ?{
  foo : number,
  'foo.bar.baz' : number,
}

obj.
//  ^
