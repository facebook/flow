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
  declare const obj: $ObjMap<SourceObj, ((string)=>number) & ((number)=>string)>;
  obj.foo;
//     ^
}

{
  declare const obj: $ObjMapConst<SourceObj, string>;
  obj.foo;
//     ^
}

{
  declare const obj: {+[key in keyof SourceObj]: SourceObj[key]};
  obj.foo;
//     ^
}
