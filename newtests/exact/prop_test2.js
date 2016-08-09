/***
 * more prop tests on unions of objects.
 * to see why property existence tests don't work to refine inexact types,
 * take a look at checkFlag_err() below.
 */

type StringFlag = {
  type: "string",
  name: string,
  description: string,
  argName: string,
  aliases?: Array<string>,
  default?: string,
};

type BoolFlag = {
  type: "boolean",
  name: string,
  description: string,
  aliases?: Array<string>,
};

type EnumFlag = {
  type: "enum",
  name: string,
  description: string,
  argName: string,
  validValues: Array<string>,
  aliases?: Array<string>,
  default?: string,
};

type Flag = StringFlag | BoolFlag | EnumFlag;

function checkFlag_err(flag: Flag): string {
  if (flag.default) {    // error, prop not found (BoolFlag)
    // if we allowed an existence test on flag.default when flag's type
    // is a tagged union of inexact object types, consider the case where
    // flag = { type: "boolean", ..., default: 0 }.
    // because subtyping allows the extra property, the implication that
    // (flag.default is truthy) implies (flag.default is a string)
    // no longer holds.
    return flag.default;
  }
  return "";
}

type ExactFlag = $Exact<StringFlag> | $Exact<BoolFlag> | $Exact<EnumFlag>;

function checkFlag_ok(flag: ExactFlag): string {
  if (flag.default) {   // ok: truthiness guarantees string type
    return flag.default;
  }
  return "";
}
