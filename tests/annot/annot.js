function foo(str:string, i:number):string {
  return str;
}
var bar: (str:number, i:number)=> string = foo;

var qux = function(str:string, i:number):number { return foo(str,i); }

var obj: {str:string; i:number; j:boolean} = {str: "...", i: "...", k: false};

var arr: Array<number> = [1,2,"..."];

// array sugar
var array: number[] = [1,2,"..."];

var matrix: number[][] = [[1,2],[3,4]];
var matrix_parens: (number[])[] = matrix;

var nullable_array: ?number[] = null;
var nullable_array_parens: ?(number[]) = nullable_array;

var array_of_nullable: (?number)[] = [null, 3];

var array_of_tuple: [number, string][] = [[0, "foo"], [1, "bar"]];
var array_of_tuple_parens: ([number, string])[] = array_of_tuple;
