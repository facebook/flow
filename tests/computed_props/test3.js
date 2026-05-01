const { ColorId } = require('./test');
const ColorIdToNumber = require('./test2');

ColorIdToNumber[ColorId.BLUE] as 'ffffff'; // oops
