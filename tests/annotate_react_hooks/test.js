import * as React from 'react';
import {useState, useRef, useCallback} from 'react';

// null
{
  const [val, setValue] = useState(null); // ANNOTATED
  setValue(1);
  const ref = useRef(null); // ANNOTATED
  ref.current = 1;
}
{
  const [val, setValue] = useState(null); // NOT ANNOTATED
  const ref = useRef(null); // NOT ANNOTATED
}

// undefined
{
  const [val, setValue] = useState(undefined); // ANNOTATED
  setValue(1);
  const ref = useRef(undefined); // ANNOTATED
  ref.current = 1;
}
{
  const [val, setValue] = useState(undefined); // NOT ANNOTATED
  const ref = useRef(undefined); // NOT ANNOTATED
}

// No-arg
{
  const [val, setValue] = useState(); // ANNOTATED
  setValue(1);
  const ref = useRef(); // ANNOTATED
  ref.current = 1;
}
{
  const [val, setValue] = useState(); // NOT ANNOTATED
  const ref = useRef(); // NOT ANNOTATED
}

// Empty array
{
  const [val, setValue] = useState([]); // ANNOTATED
  setValue([1]);
  const ref = useRef([]); // ANNOTATED
  ref.current = [1];
}
{
  const [val, setValue] = useState([]); // ANNOTATED
  val.push(1);
  const ref = useRef([]); // ANNOTATED
  ref.current.push(1);
}
{
  const [val, setValue] = useState([]); // NOT ANNOTATED
  const ref = useRef([]); // NOT ANNOTATED
}

// Empty Set
{
  const [val, setValue] = useState(new Set()); // ANNOTATED
  setValue(new Set([1, 2]));
}
{
  const [val, setValue] = useState(new Set()); // ANNOTATED
  val.add(1);
}
{
  const [val, setValue] = useState(new Set()); // NOT ANNOTATED
}

// Empty Map
{
  const [val, setValue] = useState(new Map()); // ANNOTATED
  setValue(new Map([["a", 1], ["b", 2]]));
}
{
  const [val, setValue] = useState(new Map()); // ANNOTATED
  val.set("a", 1);
}
{
  const [val, setValue] = useState(new Map()); // NOT ANNOTATED
}

{
  let f = useCallback((a, b) => {}); // ANNOTATED
  f("", 0);
}
{
  let f = React.useCallback((a, b) => {}); // ANNOTATED
  f("", 0);
}
{
  let f = useCallback((a, b) => {}, []); // ANNOTATED
  f("", 0);
}
{
  let f = React.useCallback((a, b) => {}, []); // ANNOTATED
  f("", 0);
}
{
  let f = useCallback((a, b) => {}); // ANNOTATED
  f();
}
{
  let f = React.useCallback((a, b) => {}); // ANNOTATED
  f();
}
{
  let f = useCallback((a, b) => {}); // NOT ANNOTATED
}
{
  let f = React.useCallback((a, b) => {}); // NOT ANNOTATED
}
