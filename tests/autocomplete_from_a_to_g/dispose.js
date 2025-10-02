//@flow

declare var foo : { @@dispose: void };
foo[ ]
//  ^

declare var bar : { @@asyncDispose: Promise<void> };
bar[ ]
//  ^
