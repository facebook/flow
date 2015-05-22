/**
 * @flow
 */

interface IHasXString {
  x: string;
}

var propTest1: IHasXString = { x: 'hello' };
var propTest2: IHasXString = { x: 123 }; // Error string != number
var propTest3: IHasXString = {}; // Property not found
var propTest4: IHasXString = ({}: Object);
