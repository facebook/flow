var x:string = 0;
var x:number = 1;

//declare var T: $Type<number | Array<T>>;

function foo(p: bool) {}

function sorry(really: bool) {
    if (really) {
        var x: number | string = 1337;
    } else {
        var x: bool = true;
    }
    foo(x);
}
