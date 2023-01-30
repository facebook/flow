// @jsx Foo
const Bar = 123;

{
  function Foo(
    elem: number,
    props: null,
    child1: 'hello',
    child2: boolean,
    child3: 'bye',
    ...rest: Array<void>
  ) {}
  // Whitespace trimming

  // error: hi ~> hello, bye there ~> bye
<Bar>

  hi
  {true}
  bye
  there

</Bar>;
}

{
  function Foo(
    elem: number,
    props: null,
    child1: "should be single space",
    child2: "should be true",
    child3: "should be empty string",
    child4: "should be single space",
    ...rest: Array<void>
  ) {}

  // JSXText children with only whitespace or newlines are ignored
  // error
  <Bar> {true}
  {''} </Bar>;
}

{
  // JSXText trimming
  let Foo = (elem: any, props: any, c1: "First Middle Last") => {};
  // error
  (<Bar>    First${"     "}
  Middle${"     "}
       Last     </Bar>);

  // ok
  (<Bar>First

    Middle

  Last</Bar>);

  // Tabs are turned into spaces
  (<Bar>First	Middle	Last</Bar>); // ok

  // Multiple spaces midline stay as multiple spaces
  (<Bar>First    Middle	 	 Last</Bar>); // error
}
