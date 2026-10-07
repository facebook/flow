import {ABSOLUTE_DATE_SENTINEL} from './consts';

function refines(x: ?string): string {
  libAssert(x != null);
  return x;
}

function neverReturns(c: boolean): string {
  libAssert(false);
  return "default string";
}

type OperatorConfigA = { valueType: 'ABSOLUTE_DATE', dateOnly?: boolean, ... };
type OperatorConfigS = { valueType: 'STRING', other: number, ... };
type OperatorConfig = OperatorConfigA | OperatorConfigS;

declare const configs: { oc: OperatorConfig };

function refinesDiscriminant(): boolean {
  const operatorConfig = configs.oc;
  libAssert(operatorConfig.valueType === 'ABSOLUTE_DATE');
  operatorConfig as OperatorConfigA; // narrowing check: errors if not refined
  const dateOnly = operatorConfig.dateOnly ?? false;
  return dateOnly;
}

const LOCAL_ABSOLUTE_DATE = 'ABSOLUTE_DATE';

function refinesDiscriminantAgainstLocalConst(): boolean {
  const operatorConfig = configs.oc;
  libAssert(operatorConfig.valueType === LOCAL_ABSOLUTE_DATE);
  operatorConfig as OperatorConfigA; // narrowing check: errors if not refined
  const dateOnly = operatorConfig.dateOnly ?? false;
  return dateOnly;
}

function refinesDiscriminantAgainstImportedConst(): boolean {
  const operatorConfig = configs.oc;
  libAssert(operatorConfig.valueType === ABSOLUTE_DATE_SENTINEL);
  operatorConfig as OperatorConfigA; // narrowing check: errors if not refined
  const dateOnly = operatorConfig.dateOnly ?? false;
  return dateOnly;
}

// Negated assertions inside switch cases keep both the assertion's own
// refinement and pre-existing case narrowing.
type SwitchItem = {type: 'A', fbid: string} | {type: 'B', other: number};

function reproSwitchNegatedTypeof(type: string, value: number | string): string {
  switch (type) {
    case 'stage':
      libAssert(typeof value !== 'number');
      return value;
    default:
      return 'default';
  }
}

function reproSwitchPreservation(item: SwitchItem, task: {sprintID: string}): string {
  switch (item.type) {
    case 'A':
      libAssert(task.sprintID !== item.fbid);
      return item.fbid;
    default:
      return 'default';
  }
}

// Control: a single member null-check refines fine in isolation.
function reproSingleMemberNull(field: {a: ?string}): string {
  libAssert(field.a != null);
  return field.a;
}

// The FIRST of two sequential asserts must survive the second call.
function reproTwoAssertsFirst(field: {a: ?string, b: ?string}): string {
  libAssert(field.a != null);
  libAssert(field.b != null);
  return field.a;
}

// Control: the LAST of two sequential asserts survives.
function reproTwoAssertsLast(field: {a: ?string, b: ?string}): string {
  libAssert(field.a != null);
  libAssert(field.b != null);
  return field.b;
}

// An assert on an unrelated var must not kill a member refinement.
function reproAssertUnrelated(field: {a: ?string}, y: ?string): string {
  libAssert(field.a != null);
  libAssert(y != null);
  return field.a;
}

declare function someOrdinaryCall(): void;

// Control: an ordinary call MUST still kill the refinement (callee may
// mutate). Only assertion calls get the no-havoc treatment. Permanently
// erroring.
function reproAssertThenCall(field: {a: ?string}): string {
  libAssert(field.a != null);
  someOrdinaryCall();
  return field.a;
}

// Sequential member null-checks keep every refinement.
function reproSequentialMemberNull(field: {a: ?string, b: ?string, c: ?string}): string {
  libAssert(field.a != null);
  libAssert(field.b != null);
  libAssert(field.c != null);
  return field.a + field.b + field.c;
}
