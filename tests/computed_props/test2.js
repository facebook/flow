const { ColorId, ColorNumber } = require('./test');
// TODO T64194787
const ColorIdToNumber = {
  [ColorId.RED]: ColorNumber.RED,
  [ColorId.GREEN]: ColorNumber.GREEN,
  [ColorId.BLUE]: ColorNumber.BLUE,
};

(ColorIdToNumber[ColorId.GREEN]: 'ffffff'); // oops

module.exports = ColorIdToNumber;
