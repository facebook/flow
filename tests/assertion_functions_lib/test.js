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
