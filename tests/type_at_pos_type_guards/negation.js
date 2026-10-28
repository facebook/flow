declare function isString(x: unknown): x is string;

function any_input(x: any) {
  if (!isString(x)) {
    x;
//  ^
  }
}

function any_member(x: number | any) {
  if (!isString(x)) {
    x;
//  ^
  }
}

function any_bound<T extends any>(x: T) {
  if (!isString(x)) {
    x;
//  ^
  }
}
