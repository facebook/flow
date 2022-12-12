var x:string = 0;
var x:number = 1;

function foo(p: boolean) {}

function sorry(really: boolean) {
    if (really) {
        var x: number | string = 1337;
    } else {
        var x: boolean = true;
    }
    foo(x);
}

function foo0(b: boolean): number {
  var x: number | string = 0;
  if (b) {
    x = "";
  }
  return x;  // error: string ~> number
}
