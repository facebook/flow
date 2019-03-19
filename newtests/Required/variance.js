//@flow

declare var foo: $Required<{
  +readonly?: number,
  -writeonly?: string,
  neutral?: number
}>;

foo.readonly = undefined; // error
foo.readonly = 1; // error
(foo.readonly: number);

foo.writeonly = undefined; // error
foo.writeonly = "";
(foo.writeonly: string);

foo.neutral = undefined; // error
foo.neutral = 2;
(foo.neutral: number);
