/**
 * @flow
 */
type Person<A> = { name: A };
declare const obj: Person<string>;

obj.
//  ^
