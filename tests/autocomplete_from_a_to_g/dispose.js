//@flow

declare const foo : { @@dispose: void };
foo[ ]
//  ^

declare const bar : { @@asyncDispose: Promise<void> };
bar[ ]
//  ^
