class A {}
class B extends A {};

// named properties

type RWA = {p: A}
type ROA = {+p: A}
type WOA = {-p: A}
type RWB = {p: B}
type ROB = {+p: B}
type WOB = {-p: B}

declare var rwA: RWA;
declare var roA: ROA;
declare var woA: WOA;

declare var rwB: RWB;
declare var roB: ROB;
declare var woB: WOB;

// dictionaries

type dRWA = {[string]: A};
type dROA = {+[string]: A};
type dWOA = {-[string]: A};
type dRWB = {[string]: B};
type dROB = {+[string]: B};
type dWOB = {-[string]: B};

declare var drwA: dRWA;
declare var droA: dROA;
declare var dwoA: dWOA;

declare var drwB: dRWB;
declare var droB: dROB;
declare var dwoB: dWOB;

// X ~> A
{
  // literal A
  ({p: new A} as RWA); // ok
  ({p: new A} as dRWA); // ok

  // A
  rwA as RWA; // ok
  drwA as dRWA; // ok

  // +A
  roA as RWA; // error
  droA as dRWA; // error

  // -A
  woA as RWA; // error
  dwoA as dRWA; // error

  // literal B
  ({p: new B} as RWA); // ok
  ({p: new B} as dRWA); // ok

  // B
  rwB as RWA; // error
  drwB as dRWA; // error

  // +B
  roB as RWA; // error
  droB as dRWA; // error

  // -B
  woB as RWA; // error
  dwoB as dRWA; // error
}

// X ~> +A
{
  // literal A
  ({p: new A} as ROA); // ok
  ({p: new A} as dROA); // ok

  // A
  rwA as ROA; // ok
  drwA as dROA; // ok

  // +A
  roA as ROA; // ok
  droA as dROA; // ok

  // -A
  woA as ROA; // error
  dwoA as dROA; // error

  // literal B
  ({p: new B} as ROA); // ok
  ({p: new B} as dROA); // ok

  // B
  rwB as ROA; // ok
  drwB as dROA; // ok

  // +B
  roB as ROA; // ok
  droB as dROA; // ok

  // -B
  woB as ROA; // error
  dwoB as dROA; // error
}

// X ~> -A
{
  // literal A
  ({p: new A} as WOA); // ok
  ({p: new A} as dWOA); // ok

  // A
  rwA as WOA; // ok
  rwA as dWOA; // ok

  // +A
  roA as WOA; // error
  droA as dWOA; // error

  // -A
  woA as WOA; // ok
  dwoA as dWOA; // ok

  // literal B
  ({p: new B} as WOA); // ok
  ({p: new B} as dWOA); // ok

  // B
  rwB as WOA; // error
  drwB as dWOA; // error

  // +B
  roB as WOA; // error
  droB as dWOA; // error

  // -B
  woB as WOA; // error
  dwoB as dWOA; // error
}

// X ~> B
{
  // literal A
  ({p: new A} as RWB); // error
  ({p: new A} as dRWB); // error

  // A
  rwA as RWB; // error
  drwA as dRWB; // error

  // +A
  roA as RWB; // error
  droA as dRWB; // error

  // -A
  woA as RWB; // error
  dwoA as dRWB; // error
}

// X ~> +B
{
  // literal A
  ({p: new A} as ROB); // error
  ({p: new A} as dROB); // error

  // A
  rwA as ROB; // error
  drwA as dROB; // error

  // +A
  roA as ROB; // error
  droA as dROB; // error

  // -A
  woA as ROB; // error
  dwoA as dROB; // error
}

// X ~> -B
{
  // literal A
  ({p: new A} as WOB); // ok
  ({p: new A} as dWOB); // ok

  // A
  rwA as WOB; // ok
  drwA as dWOB; // ok

  // +A
  roA as WOB; // error
  droA as dWOB; // error

  // -A
  woA as WOB; // ok
  dwoA as dWOB; // ok
}

// unification
{
  // Note: these tests don't reuse the type aliases from the prelude because
  // doing so results in "naive" unification instead of rec_unify.

  [rwA] as Array<{p:A,...}> as Array<{p:A,...}>; // ok

  [roA] as Array<{+p:A,...}> as Array<{p:A,...}>; // error

  [woA] as Array<{-p:A,...}> as Array<{p:A,...}>; // error

  [rwA] as Array<{p:A,...}> as Array<{+p:A,...}>; // error

  [roA] as Array<{+p:A,...}> as Array<{+p:A,...}>; // ok

  [woA] as Array<{-p:A,...}> as Array<{+p:A,...}>; // error

  [rwA] as Array<{p:A,...}> as Array<{-p:A,...}>; // error

  [roA] as Array<{+p:A,...}> as Array<{-p:A,...}>; // error

  [woA] as Array<{-p:A,...}> as Array<{-p:A,...}>; // ok

}

// summarized [incompatible-variance] error
{
  type T1 = {[k in 'f1'|'f2'|'f3'|'f4'|'f5']: number};

  declare var x1: Readonly<T1>;
  x1 as T1; // error f1...f5 incompatible

  type T2 = {...T1, f6: number};

  declare var x2: Readonly<T2>;
  x2 as T2; // error f1...f4 and others incompatible
}
