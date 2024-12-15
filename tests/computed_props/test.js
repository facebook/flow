const ColorId = {
  RED: 'R',
  GREEN: 'G',
  BLUE: 'B',
} as const;

const ColorNumber = {
  RED: 'ff0000',
  GREEN: '00ff00',
  BLUE: '0000ff',
} as const;

const ColorIdToNumber = {
  [ColorId.RED]: ColorNumber.RED,
  [ColorId.GREEN]: ColorNumber.GREEN,
  [ColorId.BLUE]: ColorNumber.BLUE,
};

(ColorIdToNumber[ColorId.RED]: 'ffffff'); // oops

ColorIdToNumber.XXX; // oops

module.exports = { ColorId, ColorNumber };
