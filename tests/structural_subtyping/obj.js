/**
 * @flow
 */

interface IHasXString {
  x: string;
}

var propTest1: IHasXString = {x: 'hello'};
var propTest2: IHasXString = {x: 123}; // Error string != number
var propTest3: IHasXString = {}; // Property not found

function propTest4(y: {[key: string]: string}) {
  y as IHasXString; // OK
}

function propTest5(y: {[key: string]: number}) {
  y as IHasXString; // error: string != number
}
