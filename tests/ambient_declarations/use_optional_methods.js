import {OptionalMethods} from './optional_methods';

const instance = new OptionalMethods();

// Optional method should be ((x: number) => void) | void
instance.optionalMethod as ((x: number) => void) | void; // OK
instance.optionalMethod as (x: number) => void; // ERROR: could be void

// Optional static method should be (() => string) | void
OptionalMethods.optionalStaticMethod as (() => string) | void; // OK
OptionalMethods.optionalStaticMethod as () => string; // ERROR: could be void

// Optional generic method
instance.optionalGeneric as (<T>(x: T) => T) | void; // OK
instance.optionalGeneric as <T>(x: T) => T; // ERROR: could be void

// Calling optional method requires check
if (instance.optionalMethod) {
  instance.optionalMethod(42); // OK after check
}
