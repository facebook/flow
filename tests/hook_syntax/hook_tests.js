hook useCustom<T>(x: T): [T] { return [x]; }

const [v] = useCustom({a: 42});
v.a = 100; // Error, x is not writable

useCustom as <T>(T) => [T]; // error, hook and nonhook incompatible
useCustom as hook <T>(T) => [T]; // ok

declare const nonhook: <T>(T) => [T];
nonhook as typeof useCustom; // error, nonhook and hook incompatible

hook useCustom2<T>(x: T): [T] { return [x];}
useCustom as typeof useCustom2; // error, hooks unique

useCustom as typeof useCustom; // ok

hook useCoolHook(): Array<number> {
    const [foo, ref] = useCoolHook2();
    foo.a = "hi"; // error
    return [];
}

hook useCoolHook2(): [{a: string}, React.RefObject<number>] {
    return (42: any);
}

import * as React from 'react';

component B() {
    const arr = useCoolHook();
    arr[0] = 42; // error
    const [foo, ref] = useCoolHook2();
    foo.a = "hi"; // error
    React.useEffect(() => {
        ref.current = 42; // ok
    })
    return null;
}

hook useBadHook(x: Array<number>, y: React.RefObject<number>): void {
    x[0] = 42; // error
    React.useEffect(() => {
        y.current = 42; // ok
    })
    const w = React.useRef<?number>(null);
    w.current = 42; // error
}
