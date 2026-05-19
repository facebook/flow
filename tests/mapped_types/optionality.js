type O = {foo: number, bar?: string};

// Optionality is preserved when no specifiers are present
{
  type Preserve = {[key in keyof O]: O[key]};

  declare const preserve: Preserve;
  preserve as {foo: number, bar?: string}; // OK
  preserve as {foo: string, bar?: number}; // Error string ~> number, number ~> string

  declare const lit: {foo: number, bar?: string};
  lit as Preserve; // OK
  ({foo: 'str', bar: 3} as Preserve); // ERROR
}
// +? adds optionality to all properties
{
  type Add = {[key in keyof O]+?: O[key]};
  declare const add: Add;
  add as {foo?: number, bar?: string}; // OK

  declare const lit: {foo?: number, bar?: string};
  lit as Add; // OK
}


// -? is only available under experimental.tslib_syntax
{
  type Removed = {[key in keyof O]-?: O[key]}; // ERROR - tslib_syntax not enabled
}

// -readonly + -? together both report under one pass
{
  type Both = {-readonly [key in keyof O]-?: O[key]}; // ERROR x2 - tslib_syntax not enabled
}
