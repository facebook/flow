import * as React from 'react';
import { useRef, useEffect, useCallback, useMemo } from 'react';

hook useTakeRef(ref: React.RefObject<number>) {
    ref.current = 42; // error
}

hook useTakeAnything(x: mixed) { }

function takeRef(ref: React.RefObject<number>) { }

function escape(x: mixed) { }

component H() {
    const ref = useRef(42);
    useTakeRef(ref);
    takeRef(ref); // error
    function boom() {
        ref.current = 42; // error, triggeted by boom()
    }
    boom();
    function no_call_so_no_boom() {
        ref.current = 42; // no error
    }

    escape(() => { ref.current = 42; }); // no error
    useEffect(() => { ref.current = 42; }, []); // no error
    useTakeAnything(() => { ref.current = 42; }); // no error
    useMemo(() => { ref.current = 42; }, []); // error

    function nestedBoomInner() {
        ref.current = 42; // error, triggeted by nestedBoomOuter()
    }

    function nestedBoomOuter() {
        nestedBoomInner();
    }

    nestedBoomOuter();

    function ok_in_jsx() {
        ref.current = 42; // no error
    }

    const cb_boom = useCallback(() => { ref.current = 42; }, []); // error
    const cb_no_boom_effect = useCallback(() => { ref.current = 42; }, []); // no error
    const cb_no_boom_not_called = useCallback(() => { ref.current = 42; }, []); // no error

    cb_boom();
    useEffect(cb_no_boom_effect);
    useEffect(() => cb_no_boom_effect());

    return <div onWhatever={ok_in_jsx} onAnotherThing={() => { ref.current = 42 /* ok */ }} />;
}

component Loops() {
    function a(): empty {
        return b();
    }
    function b(): empty {
        return a();
    }
    return a();
}

component R() {
    const ref = useRef(42);
    const cb = useCallback(() => { ref.current = 42; }, [ref]); // no error
    useMemo(() => cb, [ref]);
    return null;
}
