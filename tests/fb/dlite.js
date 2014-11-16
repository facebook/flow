var E = require('./E');

class C {
    static bar(x:number) { }
    qux(x:number) { }
}

var D = React.createClass({
    mixins: [C],
    statics: { foo(x:number) { } },
    xyzzy() { this.qux(0); }
});

D.foo("...");
D.bar("...");

E.queries;
E.getQuery();
E.foo();
var e = <E></E>;
e.queryParams();
e.bar();
