//@flow

declare var foo : { @@iterator: Iterator<number> };
foo[ ]
//  ^

declare var bar : { @@asyncIterator: AsyncIterator<number> };
bar[ ]
//  ^
