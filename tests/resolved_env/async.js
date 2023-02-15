//@flow

async function *f() {
    var x = await (yield* 42); // TODO: We have a spurious underconstrained error here. To fix this, we will need any propagation for instance type and default resolve after subtyping failure.
}
