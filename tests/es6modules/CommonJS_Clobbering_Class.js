/**
 * @providesModule CommonJS_Clobbering_Class
 * @flow
 */

class Test {
  static staticNumber1() { return 1; }
  static staticNumber2() { return 2; }
  static staticNumber3() { return 3; }
  static staticNumber4() { return 4; }

  instNumber1():number { return 1; }
  instNumber2():number { return 2; }
};

module.exports = Test;
