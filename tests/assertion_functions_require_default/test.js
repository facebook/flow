const esmFn = require('./esm_fn_default');
const esmObj = require('./esm_obj_default');
const esmNoDefault = require('./esm_nodefault');
const cjs = require('./cjs');

function bareFnDefault(value: unknown) {
  esmFn(value);
  value as string;
  value as number; // error
}

function memberObjDefault(value: unknown) {
  esmObj.assertNumber(value);
  value as number;
  value as string; // error
}

function cjsPrecedence(value: unknown) {
  cjs(value);
  value as string;
  value as number; // error
}

function nodefaultMember(value: unknown) {
  esmNoDefault.assertNumber(value);
  value as number;
  value as string; // error
}

function nodefaultBare(value: unknown) {
  esmNoDefault(value);
  value as string; // error: bare namespace call does not narrow
  value as number; // error
}
