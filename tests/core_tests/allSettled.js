// @flow

function* generator(): Iterable<Promise<number>> {
    while (true) {
        yield Promise.resolve(1);
    }
}

let tests = [
    // good cases
    function() {
        let p1 : Promise<[]> = Promise.allSettled([]);
        let p2 : Promise<[
            SettlementResult<number>,
            SettlementResult<string>,
            SettlementResult<boolean>,
        ]> = Promise.allSettled([1, '', false]);
        let p3 : Promise<[
            SettlementResult<number>,
            SettlementResult<string>,
            SettlementResult<boolean>,
        ]> = Promise.allSettled([
            Promise.resolve(1),
            Promise.resolve(''),
            Promise.resolve(false),
        ]);
        let p4 : Promise<[
            SettlementResult<number>,
            SettlementResult<number>,
        ]> = Promise.allSettled([
            1,
            Promise.resolve(1),
        ]);
        let p5 : Promise<$ReadOnlyArray<SettlementResult<number>>> = Promise.allSettled(generator());

        p5.then((results): Array<number> => {
            return results.map(result => {
                if (result.status === 'fulfilled') {
                    return result.value;
                } else {
                    return -1;
                }
            });
        });
        p5.then((results): Array<number> => {
            return results.map(result => {
                if (result.status === 'rejected') {
                    return -1;
                } else {
                    return result.value
                }
            });
        });
    },
    // wrong arguments
    function() {
        let p1 = Promise.allSettled();
        let p2 = Promise.allSettled(1);
        let p3 = Promise.allSettled(Promise.resolve(1));
    },
    // wrong result type
    function() {
        let p1 : Promise<void> = Promise.allSettled([]);
        let p2 : Promise<[number, string, boolean]> = Promise.allSettled([1, '', false]);
        let p3 : Promise<[number, string, boolean]> = Promise.allSettled([
            Promise.resolve(1),
            Promise.resolve(''),
            Promise.resolve(false),
        ]);
    },
    // wrong result handling
    function() {
        let p1 = Promise.allSettled(generator());

        p1.then((results): Array<number> => {
            return results.map(result => result);
        });

        p1.then((results): Array<number> => {
            return results.map(result => result.value);
        });
    },
];
