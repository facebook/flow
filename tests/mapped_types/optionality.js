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


// We do not yet support -?
{
  type Removed = {[key in keyof O]-?: O[key]}; // ERROR
}
