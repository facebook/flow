// @flow

declare const roArray: ReadonlyArray<string>;
roArray[0] as string; // error when no_unchecked_indexed_access=true
