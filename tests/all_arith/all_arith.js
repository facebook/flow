// @flow

declare opaque type T;

declare var aNumber: number;
declare var aString: string;
declare var aBigint: bigint;
declare var aDate: Date;
declare var aBoolean: boolean;
declare var aT: T;
// null
declare var anArray: Array<empty>
declare var anObject: {}
declare var aMixed: mixed;
declare var anAny: any;
declare var anEmpty: empty;

// aNumber + _
(aNumber + aNumber : number); // ok
(aNumber + aString : string); // ok
(aNumber + aBigint); // error
(aNumber + aDate); // error
(aNumber + aBoolean); // error
(aNumber + aT); // error
(aNumber + null); // error
(aNumber + anArray); // error
(aNumber + anObject); // error
(aNumber + aMixed); // error
(aNumber + anAny : any); // ok
(aNumber + anEmpty : empty); // ok

// aNumber * _
(aNumber * aNumber : number); // ok
(aNumber * aString); // error
(aNumber * aBigint); // error
(aNumber * aDate); // error
(aNumber * aBoolean); // error
(aNumber * aT); // error
(aNumber * null); // error
(aNumber * anArray); // error
(aNumber * anObject); // error
(aNumber * aMixed); // error
(aNumber * anAny : any); // ok
(aNumber * anEmpty : empty); // ok

// aString + _
(aString + aString : string); // ok
(aString + aBigint); // error
(aString + aDate); // error
(aString + aBoolean); // error
(aString + aT); // error
(aString + null); // error
(aString + anArray); // error
(aString + anObject); // error
(aString + aMixed); // error
(aString + anAny : any); // ok
(aString + anEmpty : empty); // ok

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
(aString * anAny : any); // ok
(aString * anEmpty : empty); // ok

// aBigint + _
(aBigint + aBigint : bigint); // ok
(aBigint + aDate); // error
(aBigint + aBoolean); // error
(aBigint + aT); // error
(aBigint + null); // error
(aBigint + anArray); // error
(aBigint + anObject); // error
(aBigint + aMixed); // error
(aBigint + anAny : any); // ok
(aBigint + anEmpty : empty); // ok

// aBigint * _
(aBigint * aBigint : bigint); // ok
(aBigint * aDate); // error
(aBigint * aBoolean); // error
(aBigint * aT); // error
(aBigint * null); // error
(aBigint * anArray); // error
(aBigint * anObject); // error
(aBigint * aMixed); // error
(aBigint * anAny : any); // ok
(aBigint * anEmpty : empty); // ok

// aDate + _
(aDate + aDate); // error
(aDate + aBoolean); // error
(aDate + aT); // error
(aDate + null); // error
(aDate + anArray); // error
(aDate + anObject); // error
(aDate + aMixed); // error
(aDate + anAny : any); // ok
(aDate + anEmpty : empty); // ok

// aDate * _
(aDate * aDate); // error
(aDate * aBoolean); // error
(aDate * aT); // error
(aDate * null); // error
(aDate * anArray); // error
(aDate * anObject); // error
(aDate * aMixed); // error
(aDate * anAny : any); // ok
(aDate * anEmpty : empty); // ok

// aBoolean + _
(aBoolean + aBoolean); // error
(aBoolean + aT); // error
(aBoolean + null); // error
(aBoolean + anArray); // error
(aBoolean + anObject); // error
(aBoolean + aMixed); // error
(aBoolean + anAny : any); // ok
(aBoolean + anEmpty : empty); // ok

// aBoolean * _
(aBoolean * aBoolean); // error
(aBoolean * aT); // error
(aBoolean * null); // error
(aBoolean * anArray); // error
(aBoolean * anObject); // error
(aBoolean * aMixed); // error
(aBoolean * anAny : any); // ok
(aBoolean * anEmpty : empty); // ok

// t + _
(aT + aT); // error
(aT + null); // error
(aT + anArray); // error
(aT + anObject); // error
(aT + aMixed); // error
(aT + anAny : any); // ok
(aT + anEmpty : empty); // ok

// t * _
(aT * aT); // error
(aT * null); // error
(aT * anArray); // error
(aT * anObject); // error
(aT * aMixed); // error
(aT * anAny : any); // ok
(aT * anEmpty : empty); // ok

// null + _
(null + null); // error
(null + anArray); // error
(null + anObject); // error
(null + aMixed); // error
(null + anAny : any); // ok
(null + anEmpty : empty); // ok

// null * _
(null * null); // error
(null * anArray); // error
(null * anObject); // error
(null * aMixed); // error
(null * anAny : any); // ok
(null * anEmpty : empty); // ok

// anArray + _
(anArray + anArray); // error
(anArray + anObject); // error
(anArray + aMixed); // error
(anArray + anAny : any); // ok
(anArray + anEmpty : empty); // ok

// anArray * _
(anArray * anArray); // error
(anArray * anObject); // error
(anArray * aMixed); // error
(anArray * anAny : any); // ok
(anArray * anEmpty : empty); // ok

// anObject + _
(anObject + anObject); // error
(anObject + aMixed); // error
(anObject + anAny : any); // ok
(anObject + anEmpty : empty); // ok

// anObject * _
(anObject * anObject); // error
(anObject * aMixed); // error
(anObject * anAny : any); // ok
(anObject * anEmpty : empty); // ok

// aMixed + _
(aMixed + aMixed); // error
(aMixed + anAny : any); // ok
(aMixed + anEmpty : empty); // ok

// aMixed * _
(aMixed * aMixed); // error
(aMixed * anAny : any); // ok
(aMixed * anEmpty : empty); // ok

// anAny + _
(anAny + anAny : any); // ok
(anAny + anEmpty : empty); // ok

// anAny * _
(anAny * anAny : any); // ok
(anAny * anEmpty : number); // ok

// anEmpty + _
(anEmpty + anEmpty : empty); // ok

// anEmpty * _
(anEmpty * anEmpty : number); // ok

// Extra test for >>> with bigint since it's not allowed
// aBigint >>> _
(aBigint >>> aBigint); // error
(aBigint >>> aNumber); // error
