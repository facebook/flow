type SourceObj = { foo: string, bar: number };

{
  declare const obj: $ReadOnly<SourceObj>;
  obj.foo;
//     ^
}

{
  declare const obj: $Diff<SourceObj, {bar: number}>;
  obj.foo;
//     ^
}

{
  declare const obj: $Rest<SourceObj, {bar: number}>;
  obj.foo;
//     ^
}

{
  declare const obj: {[K in keyof SourceObj]: string};
  obj.foo;
//     ^
}

{
  declare const obj: {[K in keyof SourceObj]: SourceObj[K] extends string ? number : string};;
  obj.foo;
//     ^
}

{
  declare const obj: {+[key in keyof SourceObj]: SourceObj[key]};
  obj.foo;
//     ^
}
