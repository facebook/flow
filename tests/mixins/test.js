var Bar = require('./Bar');

class Foo extends mixin(Bar) {
  m() {
    var x: string = this.x;
    this.y = "";
  }
}
