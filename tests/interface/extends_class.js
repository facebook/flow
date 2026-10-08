// An interface may extend a class, as TypeScript's `interface RegExpMatchArray
// extends Array<string>` does. Checking a value against such an interface checks
// the interface's own members and then the class's.

interface StringArrayWithExtra extends Array<string> {
  extra: number;
}

declare const strings: Array<string>;
strings as StringArrayWithExtra; // Error: `extra` is missing, and nothing else

declare const numbers: Array<number>;
numbers as StringArrayWithExtra; // Error: `extra` is missing, and `number` is not `string`

// An invariant type argument checks the class-extending interface both ways.
interface Box<T> {
  get(): T;
  set(x: T): void;
}
declare const box: Box<StringArrayWithExtra>;
box as Box<Array<string>>; // Error: `extra` is missing in `Array<string>`, and nothing else

// A class implementing such an interface needs the class's members too.
class Base {
  base: number = 0;
}
interface BaseWithExtra extends Base {
  extra: string;
}
class ImplementsWithoutBase implements BaseWithExtra { // Error: `base` is missing
  extra: string = '';
}
class ImplementsWithBase extends Base implements BaseWithExtra { // ok
  extra: string = '';
}

// A tuple's members are those of `ReadonlyArray`.
declare const tuple: [string, string];
tuple as StringArrayWithExtra; // Error: `extra` is missing, and `ReadonlyArray` is not `Array`
interface ReadonlyStringArray extends ReadonlyArray<string> {}
tuple as ReadonlyStringArray; // ok

// With several supers, each class super is checked the same way.
interface Named {
  name: string;
}
interface NamedStringArray extends Named, Array<string> {}
strings as NamedStringArray; // Error: `name` is missing, and nothing else

// A member redeclared down the class chain is checked once.
class DerivedBase extends Base {
  base: number = 1;
}
interface DerivedBaseWithExtra extends DerivedBase {
  extra: string;
}
class ImplementsNothing implements DerivedBaseWithExtra {} // Error: `extra` and `base` are missing, once each

// A class's statics are not an instance of the class super.
class Statics {
  static base: number = 0;
  static extra: string = '';
}
Statics as BaseWithExtra; // Error: `Statics` is not a `Base`

// A member narrowed by a derived class is checked against the narrowed type,
// whichever order the supers are listed in.
declare class Wide {
  readonly x: string | number;
  y: number;
}
declare class Narrow extends Wide {
  readonly x: string;
}
interface NarrowThenWide extends Narrow, Wide {}
interface WideThenNarrow extends Wide, Narrow {}
class NumberXNarrowThenWide implements NarrowThenWide { // Error: `number` is not `string`
  readonly x: number = 0;
  y: number = 0;
}
class NumberXWideThenNarrow implements WideThenNarrow { // Error: `number` is not `string`
  readonly x: number = 0;
  y: number = 0;
}

interface IGeneric<T> {
  value: T;
}
interface InterfaceBoth extends IGeneric<string>, IGeneric<number> {}
class InterfaceNumberOnly implements InterfaceBoth {
  value: number = 0; // ERROR
}

declare class CGeneric<T> {
  value: T;
}
interface ClassBoth extends CGeneric<string>, CGeneric<number> {}
interface ClassBothReversed extends CGeneric<number>, CGeneric<string> {}
class ClassNumberOnly implements ClassBoth {
  value: number = 0; // ERROR
}
class ClassNumberOnlyReversed implements ClassBothReversed {
  value: number = 0; // ERROR
}
class ClassStringOnly implements ClassBoth {
  value: string = ''; // ERROR
}
class ClassStringOnlyReversed implements ClassBothReversed {
  value: string = ''; // ERROR
}

const genericObject: ClassBoth = {value: 0}; // ERROR
const genericObjectReversed: ClassBothReversed = {value: 0}; // ERROR

const objectWithBase: BaseWithExtra = {base: 0, extra: ''}; // ok
const objectWithoutBase: BaseWithExtra = {extra: ''}; // ERROR
const objectWithWrongBase: BaseWithExtra = {base: '', extra: ''}; // ERROR
const objectWithDerivedBase: DerivedBaseWithExtra = {base: 0, extra: ''}; // ok
const objectWithoutDerivedBase: DerivedBaseWithExtra = {extra: ''}; // ERROR
const narrowObject: NarrowThenWide = {x: '', y: 0}; // ok
const narrowObjectReversed: WideThenNarrow = {x: '', y: 0}; // ok
const wideObject: NarrowThenWide = {x: 0, y: 0}; // ERROR
const wideObjectReversed: WideThenNarrow = {x: 0, y: 0}; // ERROR

declare class RedeclaredGeneric extends CGeneric<string> {
  value: string;
}
interface RedeclaredThenGeneric extends RedeclaredGeneric, CGeneric<string> {}
interface GenericThenRedeclared extends CGeneric<string>, RedeclaredGeneric {}
class NumberValueRedeclaredThenGeneric implements RedeclaredThenGeneric {
  value: number = 0; // ERROR
}
class NumberValueGenericThenRedeclared implements GenericThenRedeclared {
  value: number = 0; // ERROR
}
