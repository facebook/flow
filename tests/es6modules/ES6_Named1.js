/**
 * @providesModule ES6_Named1
 * @flow
 */

const specifierNumber1 = 1;
const specifierNumber2 = 2;
const specifierNumber3 = 3;
const groupedSpecifierNumber1 = 1;
const groupedSpecifierNumber2 = 2;

export {specifierNumber1};
export {specifierNumber2 as specifierNumber2Renamed};
export {specifierNumber3};
export {groupedSpecifierNumber1, groupedSpecifierNumber2};

export function givesANumber(): number { return 42; };
export class NumberGenerator { givesANumber(): number { return 42; }};

export const varDeclNumber1 = 1, varDeclNumber2 = 2;
export const {destructuredObjNumber} = {destructuredObjNumber: 1};
export const [destructuredArrNumber] = [1]
