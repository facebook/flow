class Field {
  x: () => string;
  static y: () => string;
}

class FieldMask extends Field {
  abstract x(): string; //ng
  abstract static y(): string; //ng
}

class Get {
  get x(): () => string { return () => "x"; }
  static get y(): () => string { return () => "static y"; }
}

class GetMask extends Get {
  abstract x(): string; //ng
  abstract static y(): string; //ng
}

class Set {
  set x(k: string): void {}
  static set y(k: string): void {}
}

class SetMask extends Set {
  abstract x(k: string): void; //ng
  abstract static y(k: string): void; //ng
}

class GetSet {
  get x(): () => string { return () => "x"; }
  set x(k: string): void {}
  static get y(): () => string { return () => "static y"; }
  static set y(k: string): void {}
}

class GetSetMask extends GetSet {
  abstract x(): string; //ng
  abstract static y(): string; //ng
}

class Method {
  x(): string { return "x"; }
  static y(): string { return "static y"; }
}

class MethodMask extends Method {
  abstract x(): string; //ng
  abstract static y(): string; //ng
}

class AbstractMethod {
  abstract x(): string;
  abstract static y(): string;
}

class AbstractMethodMask {
  abstract x(): string;
  abstract static y(): string;
}
