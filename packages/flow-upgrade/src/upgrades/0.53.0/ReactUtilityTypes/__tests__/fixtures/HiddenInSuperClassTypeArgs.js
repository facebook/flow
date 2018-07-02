class Buz {}
class Foo extends Bar<ReactElement<any>> {}

const expression1 = () =>
  class {}

const expression2 = () =>
  class extends Bar<ReactElement<any>> {}
