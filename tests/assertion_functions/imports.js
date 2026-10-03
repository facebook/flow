import assertString, {
  assertNumber,
  assertNumber as renamed,
  annotated,
  assertions,
  ExportedAssertions,
  inferred,
  inferredAlias,
  inferredObject,
  importedMappedAssertions,
  agreedUnionAssertion,
  genericAppAssertion,
} from './exports';
import * as all from './exports';
import type {ImportedAssertion, ImportedAssertionFor} from './exports';

declare const fromImportedType: ImportedAssertion;
declare const fromImportedGenericType: ImportedAssertionFor<symbol>;

function named(value: unknown) {
  assertNumber(value);
  value as number;
  value as string; // error: assertion narrowed to number
}

function renamedImport(value: unknown) {
  renamed(value);
  value as number;
  value as string; // error: renamed import is an assertion
}

function defaultImport(value: unknown) {
  assertString(value);
  value as string;
  value as number; // error: assertion narrowed to string
}

function annotatedExport(value: unknown) {
  annotated(value);
  value as number;
  value as string; // error: annotated export is an assertion
}

function nestedExport(value: unknown) {
  assertions.nested.assertNumber(value);
  value as number;
  value as string; // error: nested exported property is an assertion
}

function namespaceImport(value: unknown) {
  all.assertNumber(value);
  value as number;
  value as string; // error: namespace member is an assertion
}

function importedStaticMethod(value: unknown) {
  ExportedAssertions.assertNumber(value);
  value as number;
  value as string; // error: imported static method is an assertion
}

function importedTypeAlias(value: unknown) {
  fromImportedType(value);
  value as number;
  value as string; // error: imported type annotation is an assertion
}

function importedGenericTypeAlias(value: unknown) {
  fromImportedGenericType(value);
  value as symbol;
  value as number; // error: instantiated imported alias is an assertion
}

function importedMappedProperty(numberValue: unknown, stringValue: unknown) {
  importedMappedAssertions.number(numberValue);
  numberValue as number;
  numberValue as string; // error: imported mapped property narrows to number

  importedMappedAssertions.string(stringValue);
  stringValue as string;
  stringValue as number; // error: imported mapped property narrows to string
}

function importedAgreedUnionSilent(value: unknown) {
  agreedUnionAssertion(value);
  value as number; // error: union callees never classify, even when members agree
  value as string; // error: union callees never classify, even when members agree
}

function importedGenericApplication(value: unknown) {
  genericAppAssertion(value);
  value as symbol;
  value as string; // error: imported generic application is an assertion
}

function inferredExportsAreAssertions(
  value1: unknown,
  value2: unknown,
  value3: unknown,
) {
  inferred(value1);
  value1 as number;
  value1 as string; // error: inferred export narrows to number

  inferredAlias(value2);
  value2 as number;
  value2 as string; // error: inferred alias export narrows to number

  inferredObject.assertNumber(value3);
  value3 as number;
  value3 as string; // error: inferred object export narrows to number
}
