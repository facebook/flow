// @flow

declare function invariant(condition: unknown, ...args: Array<unknown>): void;

type FieldConfig = {
  label?: string,
  fieldKey: string,
};

// `invariant(field?.label)` should raise sketchy-null-string on the
// nullable condition, like `if` does.
function optionalMember(field: FieldConfig): string {
  invariant(field?.label, 'Field must have a label');
  return field.label;
}

type PartialConfig = {
  allocation?: number,
};

// `invariant(result.allocation)` should raise sketchy-null-number,
// like `if` does.
function memberAccess(result: PartialConfig, key: string): number {
  invariant(result.allocation, `Missing allocation in ${key}`);
  return result.allocation;
}

// Controls: the same expressions in `if` conditions DO raise sketchy errors,
// proving the exists-check machinery itself works.
function controlIfMember(result: PartialConfig): number {
  if (result.allocation) {
    return result.allocation;
  }
  return 0;
}

function controlIfOptional(field: FieldConfig): string {
  if (field?.label) {
    return field.label;
  }
  return '';
}
