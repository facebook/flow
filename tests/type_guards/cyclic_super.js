// A class whose superclass annotation names the class itself, or a class that names it back, has a
// superclass chain that loops back on itself. Refining it by a type guard must terminate.
declare var SelfRecord: Class<Self>;
class Self extends SelfRecord {
  p: string;
}

declare var LoopedA: Class<LoopB>;
declare var LoopedB: Class<LoopA>;
class LoopA extends LoopedA {}
class LoopB extends LoopedB {}

class Unrelated {
  q: number;
}

declare function isUnrelated(x: unknown): x is Unrelated;
declare function isSelf(x: unknown): x is Self;
declare function isLoopA(x: unknown): x is LoopA;

function self_loop(x: Self | Unrelated) {
  if (isUnrelated(x)) {
    x as Unrelated; // okay
  }
}

function longer_loop(x: LoopA | Unrelated) {
  if (isUnrelated(x)) {
    x as Unrelated; // okay
  }
  if (isLoopA(x)) {
    x as LoopA; // okay
  }
}

function two_loops(x: LoopA | Self) {
  if (isSelf(x)) {
    x as Self; // okay
  }
}
