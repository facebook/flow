class A {}
class B extends A {};

// named properties

type RWA = {p: A}
type ROA = {readonly p: A}
type WOA = {-p: A} // TODO: replace - with writeonly once implemented
type RWB = {p: B}
type ROB = {readonly p: B}
type WOB = {-p: B} // TODO: replace - with writeonly once implemented

declare var rwA: RWA;
declare var roA: ROA;
declare var woA: WOA;

declare var rwB: RWB;
declare var roB: ROB;
declare var woB: WOB;

// dictionaries

type dRWA = {[string]: A};
type dROA = {readonly [string]: A};
type dWOA = {-[string]: A}; // TODO: replace - with writeonly once implemented
type dRWB = {[string]: B};
type dROB = {readonly [string]: B};
type dWOB = {-[string]: B}; // TODO: replace - with writeonly once implemented

declare var drwA: dRWA;
declare var droA: dROA;
declare var dwoA: dWOA;

declare var drwB: dRWB;
declare var droB: dROB;
declare var dwoB: dWOB;

// X ~> A
{
  // literal A
  ({p: new A}: RWA); // ok
  ({p: new A}: dRWA); // ok

  // A
  (rwA: RWA); // ok
  (drwA: dRWA); // ok

  // readonly A
  (roA: RWA); // error
  (droA: dRWA); // error

  // writeonly A
  (woA: RWA); // error
  (dwoA: dRWA); // error

  // literal B
  ({p: new B}: RWA); // ok
  ({p: new B}: dRWA); // ok

  // B
  (rwB: RWA); // error
  (drwB: dRWA); // error

  // readonly B
  (roB: RWA); // error
  (droB: dRWA); // error

  // writeonly B
  (woB: RWA); // error
  (dwoB: dRWA); // error
}

// X ~> readonly A
{
  // literal A
  ({p: new A}: ROA); // ok
  ({p: new A}: dROA); // ok

  // A
  (rwA: ROA); // ok
  (drwA: dROA); // ok

  // readonly A
  (roA: ROA); // ok
  (droA: dROA); // ok

  // writeonly A
  (woA: ROA); // error
  (dwoA: dROA); // error

  // literal B
  ({p: new B}: ROA); // ok
  ({p: new B}: dROA); // ok

  // B
  (rwB: ROA); // ok
  (drwB: dROA); // ok

  // readonly B
  (roB: ROA); // ok
  (droB: dROA); // ok

  // writeonly B
  (woB: ROA); // error
  (dwoB: dROA); // error
}

// X ~> writeonly A
{
  // literal A
  ({p: new A}: WOA); // ok
  ({p: new A}: dWOA); // ok

  // A
  (rwA: WOA); // ok
  (rwA: dWOA); // ok

  // readonly A
  (roA: WOA); // error
  (droA: dWOA); // error

  // writeonly A
  (woA: WOA); // ok
  (dwoA: dWOA); // ok

  // literal B
  ({p: new B}: WOA); // ok
  ({p: new B}: dWOA); // ok

  // B
  (rwB: WOA); // error
  (drwB: dWOA); // error

  // readonly B
  (roB: WOA); // error
  (droB: dWOA); // error

  // writeonly B
  (woB: WOA); // error
  (dwoB: dWOA); // error
}

// X ~> B
{
  // literal A
  ({p: new A}: RWB); // error
  ({p: new A}: dRWB); // error

  // A
  (rwA: RWB); // error
  (drwA: dRWB); // error

  // readonly A
  (roA: RWB); // error
  (droA: dRWB); // error

  // writeonly A
  (woA: RWB); // error
  (dwoA: dRWB); // error
}

// X ~> readonly B
{
  // literal A
  ({p: new A}: ROB); // error
  ({p: new A}: dROB); // error

  // A
  (rwA: ROB); // error
  (drwA: dROB); // error

  // readonly A
  (roA: ROB); // error
  (droA: dROB); // error

  // writeonly A
  (woA: ROB); // error
  (dwoA: dROB); // error
}

// X ~> writeonly B
{
  // literal A
  ({p: new A}: WOB); // ok
  ({p: new A}: dWOB); // ok

  // A
  (rwA: WOB); // ok
  (drwA: dWOB); // ok

  // readonly A
  (roA: WOB); // error
  (droA: dWOB); // error

  // writeonly A
  (woA: WOB); // ok
  (dwoA: dWOB); // ok
}

// unification
{
  // Note: these tests don't reuse the type aliases from the prelude because
  // doing so results in "naive" unification instead of rec_unify.

  (([rwA]: Array<{p:A,...}>): Array<{p:A,...}>); // ok

  (([roA]: Array<{readonly p:A,...}>): Array<{p:A,...}>); // error

  (([woA]: Array<{-p:A,...}>): Array<{p:A,...}>); // error // TODO: replace - with writeonly

  (([rwA]: Array<{p:A,...}>): Array<{readonly p:A,...}>); // error

  (([roA]: Array<{readonly p:A,...}>): Array<{readonly p:A,...}>); // ok

  (([woA]: Array<{-p:A,...}>): Array<{readonly p:A,...}>); // error // TODO: replace - with writeonly

  (([rwA]: Array<{p:A,...}>): Array<{-p:A,...}>); // error // TODO: replace - with writeonly

  (([roA]: Array<{readonly p:A,...}>): Array<{-p:A,...}>); // error // TODO: replace - with writeonly

  (([woA]: Array<{-p:A,...}>): Array<{-p:A,...}>); // ok // TODO: replace - with writeonly

}

// summarized [incompatible-variance] error
{
  type T1 = {[k in 'f1'|'f2'|'f3'|'f4'|'f5']: number};

  declare var x1: $ReadOnly<T1>;
  x1 as T1; // error f1...f5 incompatible

  type T2 = {...T1, f6: number};

  declare var x2: $ReadOnly<T2>;
  x2 as T2; // error f1...f4 and others incompatible
}
