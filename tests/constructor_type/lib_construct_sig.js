// In a library definition `{new(): T}` is a construct signature, as in TypeScript.
new LibCtor(1).x as number; // OK
LibCtor.helper() as string; // OK
LibCtor.new(1); // ERROR: not a method named `new`
