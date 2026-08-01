// A class whose superclass annotation names the class itself has a prototype chain that
// loops back on itself. Walking it to decide `instanceof` must terminate.
declare var SelfRecord: Class<Self>;
class Self extends SelfRecord {
  p: string;
}

class Unrelated {
  q: number;
}

declare const self: Self;

if (self instanceof Unrelated) {
  self as empty; // error: Unrelated ~> empty
}

if (!(self instanceof Unrelated)) {
  self as empty; // error: Self ~> empty
}

if (self instanceof Self) {
  self as empty; // error: Self ~> empty
}

// Ending the cyclic walk at the object prototype keeps `instanceof Object` working.
if (self instanceof Object) {
  self as empty; // error: Self ~> empty
}

// Two classes whose superclass annotations name each other form a longer cycle.
declare var LoopedA: Class<LoopB>;
declare var LoopedB: Class<LoopA>;
class LoopA extends LoopedA {}
class LoopB extends LoopedB {}

declare const loop: LoopA;

if (loop instanceof Unrelated) {
  loop as empty; // error: Unrelated ~> empty
}

// A union of cyclic classes: refining to one member has to walk the others' chains too.
declare const either: LoopA | Self;

if (either instanceof Self) {
  either as empty; // error: Self ~> empty
}
