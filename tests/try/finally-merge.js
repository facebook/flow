//@flow
function f () {}

function def_assign_within_try() {
    let p: number | string = 1;
    try {
        p = 'hey';
    } catch (e) {
        p = 1;
    } finally {
        (p: number); // Error
    }
}

type Obj = { p: number | string }
function def_assign_within_try_havoc(o: Obj) {
    o.p = 1; // We make an explicit assignment so that o.p is present in all envs
    try {
        o.p = 'hey';
    } catch (e) {
        f();
        o.p = 1;
    } finally {
        (o.p: number); // Error
    }
}
