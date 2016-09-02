// @flow

export var varBool = true;
export var varBoolLiteral: true = true;
export var varNum = 42;
export var varNumLiteral: 42 = 42;
export var varStr = "asdf";
export var varStrLiteral: "asdf" = "asdf";

export function f1(p: number) {
  return "asdf";
};

export function f2(p: 42): "asdf" {
  return "asdf";
};
