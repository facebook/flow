async function *gen1(): AsyncGenerator<number, void, void> {
    yield* [1, 2, 3]; // okay
}

async function *gen2(): AsyncGenerator<number, void, void> {
    yield* await Promise.all([1, 2, 3]); // okay
}

async function *gen3(): AsyncGenerator<[string, number], void, void> {
    yield* new Map<string, number>(); // okay
}

async function *gen4(): AsyncGenerator<string, void, void> {
    yield* new Set<string>(); // okay
}

async function *gen5(): AsyncGenerator<string, void, void> {
    yield* new String(); // okay
}

async function *genError1(): AsyncGenerator<number, void, void> {
    yield* await Promise.all(["a"]); // error string ~> number
}

async function *genError2(): AsyncGenerator<number, void, void> {
    yield* 1; // error non iterable
}
