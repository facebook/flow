class A {}
class B extends A {};

// named properties

type RWA = {|p: A|}
type ROA = {|+p: A|}
type WOA = {|-p: A|}
type RWB = {|p: B|}
type ROB = {|+p: B|}
type WOB = {|-p: B|}

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
  ({p: new A}: RWA); // ok
  ({p: new A}: dRWA); // ok

  // A
  (rwA: RWA); // ok
  (drwA: dRWA); // ok

  // +A
  (roA: RWA); // error
  (droA: dRWA); // error

  // -A
  (woA: RWA); // error
  (dwoA: dRWA); // error

  // literal B
  ({p: new B}: RWA); // ok
  ({p: new B}: dRWA); // ok

  // B
  (rwB: RWA); // error
  (drwB: dRWA); // error

  // +B
  (roB: RWA); // error
  (droB: dRWA); // error

  // -B
  (woB: RWA); // error
  (dwoB: dRWA); // error
}

// X ~> +A
{
  // literal A
  ({p: new A}: ROA); // ok
  ({p: new A}: dROA); // ok

  // A
  (rwA: ROA); // ok
  (drwA: dROA); // ok

  // +A
  (roA: ROA); // ok
  (droA: dROA); // ok

  // -A
  (woA: ROA); // error
  (dwoA: dROA); // error

  // literal B
  ({p: new B}: ROA); // ok
  ({p: new B}: dROA); // ok

  // B
  (rwB: ROA); // ok
  (drwB: dROA); // ok

  // +B
  (roB: ROA); // ok
  (droB: dROA); // ok

  // -B
  (woB: ROA); // error
  (dwoB: dROA); // error
}

// X ~> -A
{
  // literal A
  ({p: new A}: WOA); // ok
  ({p: new A}: dWOA); // ok

  // A
  (rwA: WOA); // ok
  (rwA: dWOA); // ok

  // +A
  (roA: WOA); // error
  (droA: dWOA); // error

  // -A
  (woA: WOA); // ok
  (dwoA: dWOA); // ok

  // literal B
  ({p: new B}: WOA); // ok
  ({p: new B}: dWOA); // ok

  // B
  (rwB: WOA); // error
  (drwB: dWOA); // error

  // +B
  (roB: WOA); // error
  (droB: dWOA); // error

  // -B
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

  // +A
  (roA: RWB); // error
  (droA: dRWB); // error

  // -A
  (woA: RWB); // error
  (dwoA: dRWB); // error
}

// X ~> +B
{
  // literal A
  ({p: new A}: ROB); // error
  ({p: new A}: dROB); // error

  // A
  (rwA: ROB); // error
  (drwA: dROB); // error

  // +A
  (roA: ROB); // error
  (droA: dROB); // error

  // -A
  (woA: ROB); // error
  (dwoA: dROB); // error
}

// X ~> -B
{
  // literal A
  ({p: new A}: WOB); // ok
  ({p: new A}: dWOB); // ok

  // A
  (rwA: WOB); // ok
  (drwA: dWOB); // ok

  // +A
  (roA: WOB); // error
  (droA: dWOB); // error

  // -A
  (woA: WOB); // ok
  (dwoA: dWOB); // ok
}

// unification
{
  // Note: these tests don't reuse the type aliases from the prelude because
  // doing so results in "naive" unification instead of rec_unify.

  (([rwA]: Array<{p:A}>): Array<{p:A}>); // ok

  (([roA]: Array<{+p:A}>): Array<{p:A}>); // error

  (([woA]: Array<{-p:A}>): Array<{p:A}>); // error

  (([rwA]: Array<{p:A}>): Array<{+p:A}>); // error

  (([roA]: Array<{+p:A}>): Array<{+p:A}>); // ok

  (([woA]: Array<{-p:A}>): Array<{+p:A}>); // error

  (([rwA]: Array<{p:A}>): Array<{-p:A}>); // error

  (([roA]: Array<{+p:A}>): Array<{-p:A}>); // error

  (([woA]: Array<{-p:A}>): Array<{-p:A}>); // ok

}
