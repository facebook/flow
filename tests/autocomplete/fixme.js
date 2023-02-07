// @flow

declare function f(x: string): number;

// 
f(3)

// $
const x = true + f(3);

declare function g(): Promise<void>;

(async () => {
    /*  */
    g();
})

// leading 
f(3)

// incomp
f(3)
