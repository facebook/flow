class A { }
var a = new A();
var s1 = `l${a.x}r`;

function tag(strings,...values) {
    var x:number = strings[0];
    return x;
}
var s2 = tag `l${42}r`;
