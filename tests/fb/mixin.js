var o1 = {
    f():number { }
};
var o2 = {
    g():string { }
};

class M extends mixin(o1,o2) {
}

var x:number = new M().g();
