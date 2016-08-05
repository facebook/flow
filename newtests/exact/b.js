/***
 * exact object types prohibit extra properties, but
 * allow per-property subtyping in the usual cases.
 */

export type Flag = $Exact<{
  type: "boolean",
  name: string,
  description: string,
  aliases?: Array<string>,
}>;

function getFlag_ok(): Flag {
  return {
    type: "boolean",
    name: "help",
    description: "Shows this usage message",
    aliases: ["h"],   // ok, even though Array<string> != Array<string> | undefined
  };
}

function getFlag_err(): Flag {
  return {
    type: "boolean",
    name: "help",
    description: "Shows this usage message",
    aliaseses: ["h"]  // not ok, property not defined
  };
}
