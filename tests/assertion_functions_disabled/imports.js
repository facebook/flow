import {assertString, assertBare, annotated} from './exports';
import type {ExportedAssertion} from './exports';

declare const fromImportedType: ExportedAssertion;

function noRefinement(value: unknown) {
  assertString(value);
  value as string; // error: no refinement without the flag
}

function noBareRefinement(value: ?string) {
  assertBare(value);
  value as string; // error: no refinement without the flag
}

function noAnnotatedRefinement(value: unknown) {
  annotated(value);
  value as number; // error: no refinement without the flag
}

function noImportedTypeRefinement(value: unknown) {
  fromImportedType(value);
  value as boolean; // error: no refinement without the flag
}

declare var g: typeof assertString;
g = (value: unknown) => {}; // no error: the import loads as a plain function
