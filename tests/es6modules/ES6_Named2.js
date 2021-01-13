/**
 * @providesModule ES6_Named2
 * @flow
 */

const specifierNumber4 = 1;
const specifierNumber5 = 2;
const groupedSpecifierNumber3 = 1;
const groupedSpecifierNumber4 = 2;

export {specifierNumber4};
export {specifierNumber5 as specifierNumber5Renamed};
export {groupedSpecifierNumber3, groupedSpecifierNumber4};

export function givesANumber2(): number { return 42; };
export class NumberGenerator2 { givesANumber(): number { return 42; }};

export const varDeclNumber3 = 1, varDeclNumber4 = 2;
export const {destructuredObjNumber2} = {destructuredObjNumber2: 1};
export const [destructuredArrNumber2] = [1]
