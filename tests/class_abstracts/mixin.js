class Concrete1 {}
class Concrete2 {}
class Abstract1 {
  abstract m(): number;
}
class Abstract2 {
  abstract n(): number;
}

// Abstracts do not mix in.
declare class K1 extends Concrete1 mixins Concrete2 {}
declare class K2 extends Concrete1 mixins Abstract2 {} //ng
declare class K3 extends Abstract1 mixins Concrete2 {} //ng
declare class K4 extends Abstract1 mixins Abstract2 {} //ng

type T = typeof Abstract2 & typeof Abstract1;
declare var AbstractIsect: T;
declare class K5 mixins AbstractIsect {} //ng

function mix(C1, C2) {
  declare class K extends C1 mixins C2 {}
  declare var k: K;
  return k
}

let k1 = mix(Concrete1, Concrete2);
let k2 = mix(Abstract1, Concrete2); //ng
let k3 = mix(Concrete1, Abstract2); //ng
let k4 = mix(Abstract1, Abstract2); //ng

declare var mixin: $Facebookism$Mixin;
class K extends mixin(Abstract1, Abstract2) {}
