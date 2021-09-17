//@flow



function foo(b) {
    var x = b ? null: false;
    var z;
    while(b) {
        if (x == null) { z = ""; break; }
        var y:number = x; // error: boolean !~> number
    }
    var w:number = z; // 2 errors: ?string !~> number
}
