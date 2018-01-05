class Field {
  abstract x(): string;
  abstract static y(): string;
}

class FieldConcrete extends Field {
  x: () => string;
  static y: () => string;
}
let fc = new FieldConcrete;

class Get {
  abstract x(): string;
  abstract static y(): string;
}

class GetConcrete extends Get {
  get x(): () => string { return () => "x"; }
  static get y(): () => string { return () => "static y"; }
}
let gc = new GetConcrete;

class Set {
  abstract x(k: string): void;
  abstract static y(k: string): void;
}

class SetConcrete extends Set {
  set x(k: string): void {} //ng
  static set y(k: string): void {} //ng
}

class GetSet {
  abstract x(): string;
  abstract static y(): string;
}

class GetSetConcrete extends GetSet {
  get x(): () => string { return () => "x"; }
  set x(k: string): void {}
  static get y(): () => string { return () => "static y"; }
  static set y(k: string): void {}
}
let gsc = new GetSetConcrete;

class Method {
  abstract x(): string;
  abstract static y(): string;
}

class MethodConcrete extends Method {
  x(): string { return "x"; }
  static y(): string { return "static y"; }
}
let mc = new MethodConcrete;

class AbstractMethod {
  abstract x(): string;
  abstract static y(): string;
}

class AbstractMethodConcrete {
  abstract x(): string;
  abstract static y(): string;
}
let amc = new AbstractMethodConcrete; //ng
