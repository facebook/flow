const useZ = require('./exp');

const useX = 42;

import { useW } from './exp2';
import useU from './exp2';
import * as useV from './exp2';

function useT<T>() {}
declare const useS: {
    <T>(x: T): T;
}

const bag_o_hooks = {
    useR() { useT(); } // no error
};

component C(...{ useY }: { useY: () => void}) {
    useY(); // no error
    useX(); // error for calling number but no hook error
    useZ(); // no error
    useW(); // no error
    useU(); // no error
    useV(); // error for calling module but no hook error
    useT(); // no error
    useT<mixed>(); // no error
    useS(); // no error
    useS<mixed>(); // no error
    bag_o_hooks.useR(); // no error
    return 42;
}

import * as React from 'react';
hook useH() { };
declare function id<T>(x: T): T;

const C1 = () => { useH() }; // no error
const c1 = () => { useH() }; // error
const C2 = React.forwardRef<_, mixed, _>(({}: {}, ref: mixed) => { useH() }); // no error
const C3 = id(({}: {}, ref: mixed) => { useH() }); // error

const myHOC = () => () => { useH() }; // no error;
function myHOC2() {
    return function() {
        useH(); // no error
    }
}

declare const useXA: ((() => void) & () => number)

const [useXB, {w: useXC}] = ((42: any): [() => void, { w: () => void }])


{
    component AC(
        useNodeActionItems: number => Array<string>,
        useValidator?: string => boolean,
        w: { useXD: () => void}
      ) {
        const actionItems = useNodeActionItems(1); // no error
        const validator = useValidator?.(""); // no error
        useXA(); // no error
        useXB(); // no error
        useXC(); // no error
        w.useXD(); // no error
        return null;
      }
}

{
    const tests = {
        render() {
            const x = useH(); // no error
        }
    }
    declare const renderHook: any;
    renderHook(() => useH()); // no error
}
