// @flow

export var a = {
  bar(): void {}
};

export var b = {
  bar: function (): void {}
};

export var c = {
  m<T>(x: T): T { return x; }
};

export var d = {
  m: function<T>(x: T): T { return x; }
};
