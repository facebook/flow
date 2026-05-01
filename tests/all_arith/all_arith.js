declare opaque type T;

declare const aNumber: number;
declare const aString: string;
declare const aBigint: bigint;
declare const aDate: Date;
declare const aBoolean: boolean;
declare const aT: T;
declare const aWeirdStringOrNumberWithIntersection: (?boolean) & (number | string);
// null
declare const anArray: Array<empty>
declare const anObject: {}
declare const aMixed: unknown;
declare const anAny: any;
declare const anEmpty: empty;

// aNumber + _
aNumber + aNumber as number; // ok
aNumber + aString as string; // ok
aWeirdStringOrNumberWithIntersection + aNumber as string | number; // ok
aNumber + aWeirdStringOrNumberWithIntersection as string | number; // ok
aString + aWeirdStringOrNumberWithIntersection as string | number; // ok
aWeirdStringOrNumberWithIntersection +  aString as string | number; // ok
(aNumber + aBigint); // error
(aNumber + aDate); // error
(aNumber + aBoolean); // error
(aNumber + aT); // error
(aNumber + null); // error
(aNumber + anArray); // error
(aNumber + anObject); // error
(aNumber + aMixed); // error
aNumber + anAny as any; // ok
aNumber + anEmpty as empty; // ok

// aNumber * _
aNumber * aNumber as number; // ok
(aNumber * aString); // error
(aNumber * aBigint); // error
(aNumber * aDate); // error
(aNumber * aBoolean); // error
(aNumber * aT); // error
(aNumber * null); // error
(aNumber * anArray); // error
(aNumber * anObject); // error
(aNumber * aMixed); // error
aNumber * anAny as any; // ok
aNumber * anEmpty as empty; // ok

// aString + _
aString + aString as string; // ok
(aString + aBigint); // error
(aString + aDate); // error
(aString + aBoolean); // error
(aString + aT); // error
(aString + null); // error
(aString + anArray); // error
(aString + anObject); // error
(aString + aMixed); // error
aString + anAny as any; // ok
aString + anEmpty as empty; // ok

// aString * _
(aString * aString); // error
(aString * aBigint); // error
(aString * aDate); // error
(aString * aBoolean); // error
(aString * aT); // error
(aString * null); // error
(aString * anArray); // error
(aString * anObject); // error
(aString * aMixed); // error
aString * anAny as any; // ok
aString * anEmpty as empty; // ok

// aBigint + _
aBigint + aBigint as bigint; // ok
(aBigint + aDate); // error
(aBigint + aBoolean); // error
(aBigint + aT); // error
(aBigint + null); // error
(aBigint + anArray); // error
(aBigint + anObject); // error
(aBigint + aMixed); // error
aBigint + anAny as any; // ok
aBigint + anEmpty as empty; // ok

// aBigint * _
aBigint * aBigint as bigint; // ok
(aBigint * aDate); // error
(aBigint * aBoolean); // error
(aBigint * aT); // error
(aBigint * null); // error
(aBigint * anArray); // error
(aBigint * anObject); // error
(aBigint * aMixed); // error
aBigint * anAny as any; // ok
aBigint * anEmpty as empty; // ok

// aDate + _
(aDate + aDate); // error
(aDate + aBoolean); // error
(aDate + aT); // error
(aDate + null); // error
(aDate + anArray); // error
(aDate + anObject); // error
(aDate + aMixed); // error
aDate + anAny as any; // ok
aDate + anEmpty as empty; // ok

// aDate * _
(aDate * aDate); // error
(aDate * aBoolean); // error
(aDate * aT); // error
(aDate * null); // error
(aDate * anArray); // error
(aDate * anObject); // error
(aDate * aMixed); // error
aDate * anAny as any; // ok
aDate * anEmpty as empty; // ok

// aBoolean + _
(aBoolean + aBoolean); // error
(aBoolean + aT); // error
(aBoolean + null); // error
(aBoolean + anArray); // error
(aBoolean + anObject); // error
(aBoolean + aMixed); // error
aBoolean + anAny as any; // ok
aBoolean + anEmpty as empty; // ok

// aBoolean * _
(aBoolean * aBoolean); // error
(aBoolean * aT); // error
(aBoolean * null); // error
(aBoolean * anArray); // error
(aBoolean * anObject); // error
(aBoolean * aMixed); // error
aBoolean * anAny as any; // ok
aBoolean * anEmpty as empty; // ok

// t + _
(aT + aT); // error
(aT + null); // error
(aT + anArray); // error
(aT + anObject); // error
(aT + aMixed); // error
aT + anAny as any; // ok
aT + anEmpty as empty; // ok

// t * _
(aT * aT); // error
(aT * null); // error
(aT * anArray); // error
(aT * anObject); // error
(aT * aMixed); // error
aT * anAny as any; // ok
aT * anEmpty as empty; // ok

// null + _
(null + null); // error
(null + anArray); // error
(null + anObject); // error
(null + aMixed); // error
null + anAny as any; // ok
null + anEmpty as empty; // ok

// null * _
(null * null); // error
(null * anArray); // error
(null * anObject); // error
(null * aMixed); // error
null * anAny as any; // ok
null * anEmpty as empty; // ok

// anArray + _
(anArray + anArray); // error
(anArray + anObject); // error
(anArray + aMixed); // error
anArray + anAny as any; // ok
anArray + anEmpty as empty; // ok

// anArray * _
(anArray * anArray); // error
(anArray * anObject); // error
(anArray * aMixed); // error
anArray * anAny as any; // ok
anArray * anEmpty as empty; // ok

// anObject + _
(anObject + anObject); // error
(anObject + aMixed); // error
anObject + anAny as any; // ok
anObject + anEmpty as empty; // ok

// anObject * _
(anObject * anObject); // error
(anObject * aMixed); // error
anObject * anAny as any; // ok
anObject * anEmpty as empty; // ok

// aMixed + _
(aMixed + aMixed); // error
aMixed + anAny as any; // ok
aMixed + anEmpty as empty; // ok

// aMixed * _
(aMixed * aMixed); // error
aMixed * anAny as any; // ok
aMixed * anEmpty as empty; // ok

// anAny + _
anAny + anAny as any; // ok
anAny + anEmpty as empty; // ok

// anAny * _
anAny * anAny as any; // ok
anAny * anEmpty as number; // ok

// anEmpty + _
anEmpty + anEmpty as empty; // ok

// anEmpty * _
anEmpty * anEmpty as number; // ok

// Extra test for >>> with bigint since it's not allowed
// aBigint >>> _
(aBigint >>> aBigint); // error
(aBigint >>> aNumber); // error
