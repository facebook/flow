import assertString from './cjs_shim';

function defaultImportOfCjsShim(value: unknown) {
  assertString(value);
  value as string;
}
