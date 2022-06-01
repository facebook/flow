//@flow

async function *f() {
    var x = await (yield* 42);
}
