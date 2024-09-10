// @flow

declare opaque type Poly<X>;
declare opaque type K;
declare opaque type L;
declare opaque type P<X>;

declare function test1<TValue>(
  q1: K & L,
// ^?
  q2: (?P<TValue>) & L,
// ^?
  q3: Poly<K & L>,
// ^?
  q4: Poly<?(((K, L) => void) & ((L, K) => void))>,
// ^?
  q5: Poly<?(K & L)>,
// ^?
): void;

declare opaque type LongLongLongLongLongLongLongLongName;
declare opaque type AnotherLongLongLongLongLongLongLongLongName;
declare opaque type LongLongLongLongLongLongLongLongNameP<X>;

declare function test2<TValue>(
  q1: LongLongLongLongLongLongLongLongName & AnotherLongLongLongLongLongLongLongLongName,
// ^?
  q2: (?LongLongLongLongLongLongLongLongNameP<TValue>) & AnotherLongLongLongLongLongLongLongLongName,
// ^?
  q3: Poly<LongLongLongLongLongLongLongLongName & AnotherLongLongLongLongLongLongLongLongName>,
// ^?
  q4: Poly<?(((LongLongLongLongLongLongLongLongName, AnotherLongLongLongLongLongLongLongLongName) => void))>,
// ^?
  q4a: Poly<?(((LongLongLongLongLongLongLongLongName, AnotherLongLongLongLongLongLongLongLongName) => void) & (AnotherLongLongLongLongLongLongLongLongName, LongLongLongLongLongLongLongLongName) => void)>,
// ^?
  q5: Poly<?(LongLongLongLongLongLongLongLongName & AnotherLongLongLongLongLongLongLongLongName)>,
// ^?
): void;
