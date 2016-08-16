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

type Flag = $Exact<StringFlag> | $Exact<BoolFlag> | $Exact<EnumFlag>;

function checkFlag_ok(flag: Flag): string {
  if (flag.default) {
    return flag.argName; // ok, refined to $Exact<StringFlag> | $Exact<EnumFlag>
  }
  return "";
}
