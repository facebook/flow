import {useId, useState} from 'react';

// Can cast to functions
useId as () => string;


// can call outside of components
const [state, setState] = useState({foo: 3});

// can modify state
state.foo = 4;

hook useFoo(prop: {foo: number}) {
    prop.foo = 4; // OK
}
