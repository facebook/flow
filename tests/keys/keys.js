/* @flow */

function testKeysOfObject(str: string, lit: 'hi') {
  str as keyof Object; // Any string should be fine
  if (str) {
    str as keyof Object; // No error, truthy string should be fine
  }
  'hi' as keyof Object; // String literal should be fine

  123 as keyof Object; // Error: number -> keys of Object
}

type StrDict = {[key: string]: mixed};
function testKeysOfStrDict(str: string, lit: 'hi') {
  str as keyof StrDict; // Any string should be fine
  if (str) {
    str as keyof StrDict; // No error, truthy string should be fine
  }
  'hi' as keyof StrDict; // String literal should be fine

  123 as keyof StrDict; // Error: number -> keys of StrDict
}

type StrLitDict = {[key: 'hi']: mixed};
function testKeysOfStrLitDict(str: string, lit: 'hi') {
  str as keyof StrLitDict; // Error: Not all strings are allowed
  if (str) {
    str as keyof StrLitDict; // Error: Not all truthy strings are allowed
  }
  'hi' as keyof StrLitDict; // The right string literal is allowed
  'bye' as keyof StrLitDict; // Error: The wrong string literal is not allowed

  123 as keyof StrLitDict; // Error: number -> keys of StrLitDict
}

type ObjLit = {hi: mixed};
function testKeysOfOtherObj(str: string, lit: 'hi') {
  str as keyof ObjLit; // Error: string -> keys of ObjLit
  if (str) {
    str as keyof ObjLit; // Error: truthy string -> keys of ObjLit
  }
  'hi' as keyof ObjLit; // String literal should be fine

  123 as keyof ObjLit; // Error: number -> keys of ObjLit
}
