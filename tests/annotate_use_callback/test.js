import * as React from 'react';
import {useCallback} from "react";

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
