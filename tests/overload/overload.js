var x1: number = "".match(0)[0];
var x2: number = "".match(/pattern/)[0];
var x3: string = "".replace(/pattern/,"...");
var x4: number = "".split(/pattern/)[0];

declare class C {
    set(x:number, y:number): void;
    set(x:string, y:number): void;
}

var a = new C();
a.set(0,1);

/********** tests **************
interface Dummy<T> {
    dumb(foo: (x:number) => number):number;
    dumb(foo: (x:string) => string):string;

    dumber<U>(bar: (x:T) => Array<U>):U;
    dumber<U>(bar: (x:T) => U):Array<U>;
}

function foo(x:string):string { return x; }
var y:number = new Dummy().dumb(foo);

function bar1(x:number):Array<string> { return []; }
var z1:number = new Dummy().dumber(bar1);

function bar2(x:number):string { return "..."; }
var z2:Array<string> = new Dummy().dumber(bar2);
*/
