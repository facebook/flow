//@flow

declare const foo : { @@iterator: Iterator<number> };
foo[ ]
//  ^

declare const bar : { @@asyncIterator: AsyncIterator<number> };
bar[ ]
//  ^

function takesMap1(map: Map<number, string>) {
    map.si
//        ^
}

function takesMap2(map: Map<number, string>) {
    map.size
//        ^
}
