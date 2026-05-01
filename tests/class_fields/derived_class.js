class Base {
  base_annotatedField: number;
  base_initializedField = 42;
  base_initializedFieldWithThis: number = this.base_initializedField;
  base_annotatedInitializedFieldValid: ?number = 42;
  base_annotatedInitializedFieldInvalid: number = 'asdf'; // Error: string ~> number

  static base_annotatedField: number;
  static base_initializedField = 'asdf';
  static base_initializedFieldWithThis: string = this.base_initializedField;
  static base_annotatedInitializedFieldValid: ?number = 42;
  static base_annotatedInitializedFieldInvalid: number = 'asdf'; // Error: string ~> number

  inherited_initializer = 42;
  static inherited_initializer = 42;
}

class Child extends Base {
  child_annotatedField: number;
  child_initializedField = 42;
  child_initializedFieldWithThis: number = this.child_initializedField;
  child_annotatedInitializedFieldValid: ?number = 42;
  child_annotatedInitializedFieldInvalid: number = 'asdf'; // Error: string ~> number

  static child_annotatedField: number;
  static child_initializedField = 'asdf';
  static child_initializedFieldWithThis: string = this.child_initializedField;
  static child_annotatedInitializedFieldValid: ?number = 42;
  static child_annotatedInitializedFieldInvalid: number = 'asdf'; // Error: string ~> number

  inherited_initializer: number;
  static inherited_initializer: number;
}

var o = new Child();

/**
 * Annotated (but uninitialized) fields still have a type.
 */
o.base_annotatedField as number;
o.base_annotatedField as string; // Error: number ~> string
Child.base_annotatedField as number;
Child.base_annotatedField as string; // Error: number ~> string

o.child_annotatedField as number;
o.child_annotatedField as string; // Error: number ~> string
Child.child_annotatedField as number;
Child.child_annotatedField as string; // Error: number ~> string

/**
 * Initialized (but unannotated) fields assume the type of their initializer.
 */
o.base_initializedField as number;
o.base_initializedField as string; // Error: number ~> string
Child.base_initializedField as string;
Child.base_initializedField as number; // Error: string ~> number

o.child_initializedField as number;
o.child_initializedField as string; // Error: number ~> string
Child.child_initializedField as string;
Child.child_initializedField as number; // Error: string ~> number

/**
 * Initialized fields can reference `this`.
 */
o.base_initializedFieldWithThis as number;
o.base_initializedFieldWithThis as string; // Error: number ~> string
Child.base_initializedFieldWithThis as string;
Child.base_initializedFieldWithThis as number; // Error: string ~> number

o.child_initializedFieldWithThis as number;
o.child_initializedFieldWithThis as string; // Error: number ~> string
Child.child_initializedFieldWithThis as string;
Child.child_initializedFieldWithThis as number; // Error: string ~> number

/**
 * Initialized + annotated fields take the type of the annotation.
 * (Note that this matters when the annotation is more general than the type of
 *  the initializer)
 */
o.base_annotatedInitializedFieldValid as ?number;
o.base_annotatedInitializedFieldValid as number; // Error: ?number ~> number
Child.base_annotatedInitializedFieldValid as ?number;
Child.base_annotatedInitializedFieldValid as number; // Error: ?number ~> number

o.child_annotatedInitializedFieldValid as ?number;
o.child_annotatedInitializedFieldValid as number; // Error: ?number ~> number
Child.child_annotatedInitializedFieldValid as ?number;
Child.child_annotatedInitializedFieldValid as number; // Error: ?number ~> number

/**
 * Initialized + annotated fields where the init/annot combo is a mismatch
 * should assume the type of the annotation.
 *
 * (This happens in addition to erroring at the site of initialization)
 */
o.base_annotatedInitializedFieldInvalid as number;
o.base_annotatedInitializedFieldInvalid as string; // Error: number ~> string
Child.base_annotatedInitializedFieldInvalid as number;
Child.base_annotatedInitializedFieldInvalid as string; // Error: number ~> string

o.child_annotatedInitializedFieldInvalid as number;
o.child_annotatedInitializedFieldInvalid as string; // Error: number ~> string
Child.child_annotatedInitializedFieldInvalid as number;
Child.child_annotatedInitializedFieldInvalid as string; // Error: number ~> string

/**
 * Derived fields without an initializer that shadow base fields *with* an
 * initializer should have the type of the base field.
 */
o.inherited_initializer as number;
o.inherited_initializer as string; // Error: number ~> string
Child.inherited_initializer as number;
Child.inherited_initializer as string; // Error: number ~> string
