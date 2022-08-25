//@flow



function foo(b: boolean) {
    var x = b ? null: false;
    var z;
    while(b) {
        if (x == null) { z = ""; break; }
        var y:number = x; // error: boolean !~> number
    }
    var w:number = z; // 2 errors: ?string !~> number
}


declare var a: string | null;
function havoc() {  a = null; }
function bar() {
  if (a == null) throw ''

  while (true) {
      (a: string); // Error
      havoc();
  }
}
