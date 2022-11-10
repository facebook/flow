//@flow

type W<X> = {x: X};

declare function use<X>(): X;

function f<XXX>(): XXX {
    const h = use();
    return h;
}
