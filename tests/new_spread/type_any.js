type O1 = {...any, ...};
var o1: O1 = 0 as unknown; // ok
o1 as empty; // ok

type O2 = {...Object, ...};
var o2: O2 = 0 as unknown; // ok
o2 as empty; // ok

declare var Base: any;
declare class Derived extends Base {}
type O3 = {...Derived, ...};
var o3: O3 = 0 as unknown; // ok
o3 as empty // ok
