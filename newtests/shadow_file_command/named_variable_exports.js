// @flow

export const constExport = 42;
export let letExport = 43;
export var varExport = 44;

export type typeExport = number;

type UnexportedT = string;
export const unexportedAlias = ((0: any): UnexportedT);

class C {}
export const unexportedNominal = ((0: any): C);
