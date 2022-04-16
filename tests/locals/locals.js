var x:string = 0;
var x:number = 1;

function foo(p: bool) {}

function sorry(really: bool) {
    if (really) {
        var x: number | string = 1337;
    } else {
        var x: bool = true;
    }
    foo(x);
}

function foo0(b: bool): number {
  var x: number | string = 0;
  if (b) {
    var x = "";
  }
  return x;  // error: string ~> number
}
