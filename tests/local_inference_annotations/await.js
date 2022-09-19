// @flow

async function test() {
    const f: (string) => void = await ((s) => {}); // ok
}
