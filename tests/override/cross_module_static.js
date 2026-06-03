// Static-side inherited walk must concretize the imported InstanceT's
// `.static` slot — cross-module InstanceTs commonly arrive with their
// static field wrapped (AnnotT etc.). Without concretization, the wrapped
// shape fell through `collect_static`'s `_ -> ()` branch and ALL of the
// ancestor's static members silently disappeared from the inherited set.

import {Counter} from './cross_module_static_lib';

class MyCounter extends Counter {
  static override reset(): void {} // OK: inherited from imported Counter's static
  static override missing(): void {} // ERROR: no such static on Counter
}

MyCounter.reset();
