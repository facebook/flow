class Base {
  annotatedField: number;
  initializedField = 42;
  initializedFieldWithThis: number = this.initializedField;
  annotatedInitializedFieldValid: ?number = 42;
  annotatedInitializedFieldInvalid: number = 'asdf'; // Error: string ~> number

  static annotatedField: number;
  static initializedField = 'asdf';
  static initializedFieldWithThis: string = this.initializedField;
  static annotatedInitializedFieldValid: ?number = 42;
  static annotatedInitializedFieldInvalid: number = 'asdf'; // Error: string ~> number
}

var o = new Base();

/**
 * Annotated (but uninitialized) fields still have a type.
 */
o.annotatedField as number;
o.annotatedField as string; // Error: number ~> string
Base.annotatedField as number;
Base.annotatedField as string; // Error: number ~> string

/**
 * Initialized (but unannotated) fields assume the type of their initializer.
 */
o.initializedField as number;
o.initializedField as string; // Error: number ~> string
Base.initializedField as string;
Base.initializedField as number; // Error: string ~> number

/**
 * Initialized fields can reference `this`.
 */
o.initializedFieldWithThis as number;
o.initializedFieldWithThis as string; // Error: number ~> string
Base.initializedFieldWithThis as string;
Base.initializedFieldWithThis as number; // Error: string ~> number

/**
 * Initialized + annotated fields take the type of the annotation.
 * (Note that this matters when the annotation is more general than the type of
 *  the initializer)
 */
o.annotatedInitializedFieldValid as ?number;
o.annotatedInitializedFieldValid as number; // Error: ?number ~> number
Base.annotatedInitializedFieldValid as ?number;
Base.annotatedInitializedFieldValid as number; // Error: ?number ~> number

/**
 * Initialized + annotated fields where the init/annot combo is a mismatch
 * should assume the type of the annotation.
 *
 * (This happens in addition to erroring at the site of initialization)
 */
o.annotatedInitializedFieldInvalid as number;
o.annotatedInitializedFieldInvalid as string; // Error: number ~> string
Base.annotatedInitializedFieldInvalid as number;
Base.annotatedInitializedFieldInvalid as string; // Error: number ~> string
