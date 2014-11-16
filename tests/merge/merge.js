/** @flow */

//var mergeObjects = require('mergeObjects');

function myMerge(
    a: {},
    b: ?{}
): { } {
    return mergeObjects(a, b);
}

function myMerge2(
    a: { key1: string; key2: number; },
    b: ?{ key3: string; },
    c: { key1: number; }
): { key1: $Either<number,string>; key2: number; key3: string; } {
    return mergeObjects(a, b, c);
}
