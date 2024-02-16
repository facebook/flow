declare const useCustom: hook <T>(x: T) => [T]

const [v] = useCustom({a: 42}); // error, not in component or hook
v.a = 100; // Error, x is not writable

useCustom as <T>(T) => [T]; // error, hook and nonhook incompatible
useCustom as hook <T>(T) => [T]; // ok

declare const nonhook: <T>(T) => [T];
nonhook as typeof useCustom; // error, nonhook and hook incompatible

declare const useCustom2: hook <T>(x: T) => [T];
useCustom as typeof useCustom2; // ok, hook types compatible

useCustom as typeof useCustom; // ok

import * as React from 'react';
import * as HookReact from './hookReact';

declare const useBadHook: hook (x: Array<number>, y: React.RefObject<number>) => void;

function nonHook() {
    useBadHook([], (42: any)); // error
    const badName = useBadHook;
    badName([], (42: any)); // error

    function nonHookInternal() { }
    const useNonHookInternal = nonHookInternal;
    useNonHookInternal(); // ok
}

component C() {
    const badName = useBadHook;
    badName([], (42: any)); // error

    if (42) {
        useBadHook([], (42: any)); // error
    }

    let f;
    if (42) {
        f = useBadHook
    } else {
        function badhook(x: Array<number>, y: React.RefObject<number>): void { }
        f = badhook
    }
    f([], (42: any)) // error

    const useBadName = nonHook;
    useBadName(); // error
    return null;
}

hook useC() {
    const badName = useBadHook;
    badName([], (42: any)); // error

    if (42) {
        useBadHook([], (42: any)); // error
    }

    let f;
    if (42) {
        f = useBadHook
    } else {
        function badhook(x: Array<number>, y: React.RefObject<number>): void { }
        f = badhook
    }
    f([], (42: any)) // error

    const useBadName = nonHook;
    useBadName(); // error
}

declare const R: ReturnType<typeof useC>;
R as empty; // error;
R as void; // ok;
