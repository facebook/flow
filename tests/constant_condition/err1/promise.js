async function _async_scope() {
    let p0: Promise<0> = Promise.resolve(0);
    if(p0) {} // Error, using unresolved promise
    if(await p0) {} // OK, we don't check on await

    let p1: Promise<number> = Promise.resolve(1);
    if(p1) {} // Error, using unresolved promise
    if(await p1) {} // OK, we don't check on await


    let p2: Promise<boolean> = Promise.resolve(true);
    if(!!!p2) {} // Error, using unresolved promise
    if(!(await p2)) {} // OK, we don't check on await

    let p3: Promise<null> = Promise.resolve(null);
    if(p3) {} // Error, using unresolved promise
    if(await p3) {} // OK, we don't check on await

    let p4: Promise<void> = Promise.resolve();
    if(p4) {} // Error, using unresolved promise
    if(await p4) {} // OK, we don't check on await

    let p5: Promise<true> = Promise.resolve(true);
    if(p5) {} // Error, using unresolved promise
    if(await p5) {} // OK, we don't check on await

    let p6: Promise<false> = Promise.resolve(false);
    if(p6) {} // Error, using unresolved promise
    if(await p6) {} // OK, we don't check on await

    let p7: ?Promise<null> = Promise.resolve(null);
    if(p7) {} // OK, nullable Promise could be checked
    if(await p7) {} // OK, we don't check on await

    let p8: ?Promise<false> = Promise.resolve(false);
    if(p8) {} // OK, nullable Promise could be checked
    if(await p8) {} // OK, we don't check on await

    let p9: ?Promise<true> = Promise.resolve(true);
    if(p9) {} // OK, nullable Promise could be checked
    if(await p9) {} // OK, we don't check on await
}

async function _async_scope_1() {
    class Promise {
        fake_promise: string;
    }
    let p1: Promise = new Promise();
    if(p1) {} // OK, it's not built-in Promise class
    if(await p1) {} // OK, it's not built-in Promise class
}
