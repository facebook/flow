import * as React from 'react';
import {useState} from 'react';

// Empty object
{
  const [val, setValue] = useState({}); // ANNOTATED
  val.a = 1;
}
{
  const [val, setValue] = React.useState({}); // ANNOTATED
  val.a = 1;
}
{
  const [val, setValue] = useState({}); // ANNOTATED
  const f = (x: {a: number}) => x.a;
  f(val);
}
{
  const [val, setValue] = useState({}); // NOT ANNOTATED
  const s: string = 'hi';
  val[s] = 1;
}
{
  const [val, setValue] = useState({}); // NOT ANNOTATED
}
type T = [string, string => string];
{
  const [val, setValue]: T = useState({}); // NOT ANNOTATED
}

// null
{
  const [val, setValue] = useState(null); // ANNOTATED
  setValue(1);
}
{
  const [val, setValue] = useState(null); // NOT ANNOTATED
}

// undefined
{
  const [val, setValue] = useState(undefined); // ANNOTATED
  setValue(1);
}
{
  const [val, setValue] = useState(undefined); // NOT ANNOTATED
}

// Empty array
{
  const [val, setValue] = useState([]); // ANNOTATED
  setValue([1]);
}
{
  const [val, setValue] = useState([]); // ANNOTATED
  val.push(1);
}
{
  const [val, setValue] = useState([]); // NOT ANNOTATED
}
