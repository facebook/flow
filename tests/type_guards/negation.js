function test0() {
  declare const isFstParamNum: (value: unknown, extra_value: unknown) => value is number;

  declare const x: number | string;
  declare const y: number | string;

  if (isFstParamNum(x, y)) {
    return;
  }
  x as string; // okay (all LatentP refinements are negated together)
}

function test1() {
  declare const obj: { check: (value: unknown) => value is number };

  declare const x: number | string;
  if (obj.check(x)) {
    x as number;
    return;
  }
  x as string; // okay (all LatentP/LatentThisPs refinements are negated together)
}

function boolean_input(x: boolean) {
  declare function isTrue(value: unknown): value is true;
  if (isTrue(x)) {
    x as true; // okay
  } else {
    x as false; // okay
    x as true; // error false ~> true
  }
}

function boolean_union_input(x: boolean | string) {
  declare function isFalse(value: unknown): value is false;
  if (!isFalse(x)) {
    x as true | string; // okay
  }
}

function maybe_boolean_input(x: ?boolean) {
  declare function isTrue(value: unknown): value is true;
  if (!isTrue(x)) {
    x as ?false; // okay
    x as ?true; // error false ~> true
  }
}

function boolean_bound<T extends boolean>(x: T) {
  declare function isTrue(value: unknown): value is true;
  if (!isTrue(x)) {
    x as false; // okay
  }
}
