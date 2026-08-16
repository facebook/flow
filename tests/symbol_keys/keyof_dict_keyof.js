// A dictionary key that is itself a key set stays compact while computing the
// dictionary's keys.

declare const sym: unique symbol;

type Source = {
  str: number,
  [sym]: string,
};

type SourceKeys = keyof Source;
type Dict = {[key: SourceKeys]: boolean};

declare const key: keyof Dict;

key as SourceKeys; // OK
key as "str" | typeof sym; // OK

// Keep the inner key set unresolved while the outer key set is built. This is
// the path where the dictionary-key use must wait without flattening the same
// `KeysT` again.
declare function keysOfKeyedDict<O>(o: O): keyof {
  [key: keyof O]: boolean,
};

declare const source: Source;
const genericKey = keysOfKeyedDict(source);

genericKey as SourceKeys; // OK
genericKey as "str" | typeof sym; // OK

// The same shape across a module boundary stays as `KeysT` while annotation
// inference builds the dictionary and its outer key set.
import type {
  DefinitionKey,
  DictKeyedByKeySet,
} from './deferred_key';
import {s as importedS} from './deferred_key';

declare const importedKey: keyof DictKeyedByKeySet;

importedKey as DefinitionKey; // OK
importedKey as "str" | typeof importedS; // OK
