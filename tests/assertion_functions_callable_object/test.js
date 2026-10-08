import assertMod from 'callable-assert';

declare const assertObj: {
  (value: unknown): asserts value,
  extra: number,
};

function localCallableObject(value: ?string) {
  assertObj(value != null);
  value as string;
}

function importedCallableObject(value: ?string) {
  assertMod(value != null);
  value as string;
}
