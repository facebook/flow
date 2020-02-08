//@flow

function fn1(x: ?number) { }

function fn2(x: number, y: number) { }

function fn3(x) {}

function fn4(x: number, y) {}

function fn5(x: ?number, y: string) {}

function fn6(x: ?number, y: ?string) {}

function fn7(x: ?number | ?string) {}

function fn8(x: null | number | string) {}

function fn9(x: void | number | string) {}

function fn10(x: void | number | string | (() => void)) {}

function fn11(x: ((x: ?string) => void)) {}

function fn12(x: (?string => void)) {}

const fn13 = (x: ?string) => {}

function fn14(x: mixed) {}

function fn15(x: void) {}

function fn16(x: any) {}

function fn17(x: ?(() => null)) {}

var fn18: { (): string } = (x) => "hi"

function fn19(x: { x: number } & { y: string }) {}

function fn20(x: (?string => void)) {}

function fn21(x: any) {}

function fn22(x: number, ...rest: string[]) {}

function fn23(x: ?number, ...rest: string[]) {}

function fn24(f: (x: number, ...rest: string[]) => null) {}

function fn25(f: (x: ?number, ...rest: string[]) => null) {}
