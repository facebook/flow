/* @flow */

function testKeysOfObject(str: string, lit: 'hi') {
  str as $Keys<Object>; // Any string should be fine
  if (str) {
    str as $Keys<Object>; // No error, truthy string should be fine
  }
  'hi' as $Keys<Object>; // String literal should be fine

  123 as $Keys<Object>; // Error: number -> keys of Object
}

type StrDict = {[key: string]: mixed};
function testKeysOfStrDict(str: string, lit: 'hi') {
  str as $Keys<StrDict>; // Any string should be fine
  if (str) {
    str as $Keys<StrDict>; // No error, truthy string should be fine
  }
  'hi' as $Keys<StrDict>; // String literal should be fine

  123 as $Keys<StrDict>; // Error: number -> keys of StrDict
}

type StrLitDict = {[key: 'hi']: mixed};
function testKeysOfStrLitDict(str: string, lit: 'hi') {
  str as $Keys<StrLitDict>; // Error: Not all strings are allowed
  if (str) {
    str as $Keys<StrLitDict>; // Error: Not all truthy strings are allowed
  }
  'hi' as $Keys<StrLitDict>; // The right string literal is allowed
  'bye' as $Keys<StrLitDict>; // Error: The wrong string literal is not allowed

  123 as $Keys<StrLitDict>; // Error: number -> keys of StrLitDict
}

type ObjLit = {hi: mixed};
function testKeysOfOtherObj(str: string, lit: 'hi') {
  str as $Keys<ObjLit>; // Error: string -> keys of ObjLit
  if (str) {
    str as $Keys<ObjLit>; // Error: truthy string -> keys of ObjLit
  }
  'hi' as $Keys<ObjLit>; // String literal should be fine

  123 as $Keys<ObjLit>; // Error: number -> keys of ObjLit
}
