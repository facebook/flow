//@flow

async function *f() {
    var x = await (yield* 42); // error: number is not iterable
}
