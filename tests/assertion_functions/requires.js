const assertString = require('./cjs_exports');
var varAssertString = require('./cjs_exports');
const templateAssertString = require(`./cjs_exports`);
const assertions = require('./cjs_object');
const esModule = require('./exports');

function cjsRequire(value: unknown) {
  assertString(value);
  value as string;
  value as number; // error
}

function templateRequire(value: unknown) {
  templateAssertString(value);
  value as string;
  value as number; // error
}

const multiArg = require('./cjs_exports', 'extra');

function multiArgRequire(value: unknown) {
  multiArg(value);
  value as string; // error: a multi-arg require is not a module binding
  value as number; // error
}

function varRequire(value: unknown) {
  varAssertString(value);
  value as string;
  value as number; // error
}

function cjsMember(value: unknown) {
  assertions.assertNumber(value);
  value as number;
  value as string; // error
}

function cjsBareMember(value: ?number) {
  assertions.assertTruth(value != null);
  value as number;
  value as string; // error
}

// `require` of an ES module is its namespace object, not its default export:
// members resolve like namespace imports.
function esNamespaceMember(value: unknown) {
  esModule.assertNumber(value);
  value as number;
  value as string; // error: narrowed to number via the named export
}

function esDefaultMember(value: unknown) {
  esModule.default(value);
  value as string;
  value as number; // error: narrowed to string via the default export
}

function esBareCall(value: unknown) {
  esModule(value);
  value as string; // error: a bare namespace call is not an assertion
  value as number; // error
}

declare function noop(value: unknown): void;

function shadowedRequire(value: ?number) {
  function require(path: string): {assertTruth: (value: unknown) => void} {
    return {assertTruth: noop};
  }
  const local = require('./cjs_object');
  local.assertTruth(value != null);
  value as number; // error: a local `require` is not the module loader
}
