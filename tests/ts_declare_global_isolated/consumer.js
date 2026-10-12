// @flow

globalBox.value as string;
globalBox.value as number; // ERROR
globalBox.count as number;
globalBox.second as boolean;
globalThis.globalBox.value as string;
new GlobalClass().field as string;
GlobalNamespace.member as boolean;
GlobalNamespace.secondMember as number;
globalOverload("value") as string;
globalOverload(1) as number;
globalOverload(true); // ERROR


declare const iterator: ScopedIterator<number>;
iterator.next() as number;
const mapped = ScopedIterator.from(iterator).map((value: number): string => "value");
mapped.next() as string;
mapped.next() as number; // ERROR

declare const constructor: ScopedIteratorConstructor; // ERROR
