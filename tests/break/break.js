function foo(b) {
    var x = b ? null: false;
    var z;
    while(b) {
        if (x == null) { z = ""; break; }
        var y:number = x;
    }
    var w:number = z;
}

function bar(b) {
    var x = b ? null: false;
    if (x == null) return;
    switch ("") {
    case 0:
        var y:number = x;
        x = "";
    case 1:
        var z:number = x;
        break;
    case 2:
    }
    var w:number = x;
}

function qux(b) {
    var z = 0;
    while(b) {
        var y:number = z;
        if (b) { z = ""; continue; }
        z = 0;
    }
    var w:number = z;
}
