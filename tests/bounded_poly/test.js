function foo<T extends number>(x: T): T {
    var _ = x * 1; // OK
    var y: string = x; // error
    return x; // OK
}

class C<T extends number> {
    bar<U extends number>(x: U): T {
        return x; // error, since T: number and U: number does not imply U: T
    }
    qux<U extends T>(x: U): T {
        var _ = x * 1; // OK, since T: number and U: T implies U: number
        var y: string = x; // error
        return x; // OK, since U: T
    }
}

function example<T extends {x: number, ...}>(o: T): T { o.x = 0; return o; }
var obj1: {x: number; y: string, ...} = example({x: 0, y: ""});
var obj2: {x: number, ...} = example({x: 0});

var c: C<string> = new C; // error, since T = string is incompatible with number
var q: number = c.qux(0);
/* 2 more errors, since argument U = number is incompatible with T = string, and
 * result T = string is incompatible with number */
