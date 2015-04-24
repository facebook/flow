/**
 * @flow
 */

//////////////////////////////////
// == typeof <<class value>> == //
//////////////////////////////////

class MyClass1 {
  getNumber(): number { return 42; }
}
var a: MyClass1 = new MyClass1();
var b: MyClass1 = MyClass1; // Error: Class<MyClass1> ~> Instance<MyClass1>

class MyClass2 {
  getNumber1(): number { return 42; }
}
// TODO: typeof on classes is busted right now
var c: typeof MyClass2 = new MyClass2(); // Error: Instance<MyClass2> ~> Class<MyClass2>

//////////////////////////////////////
// == typeof <<non-class value>> == //
//////////////////////////////////////

var numValue:number = 42;
var d: typeof numValue = 100;
var e: typeof numValue = 'asdf'; // Error: string ~> number

/////////////////////////////////
// == typeof <<type-alias>> == //
/////////////////////////////////

type numberAlias = number;
// TODO: This should be an error once Task(6860853) is completed
var f: typeof numberAlias = 42; // Error: 'typeof <<type-alias>>' makes no sense...
