import * as React from 'react';
import {useState} from 'react';

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
