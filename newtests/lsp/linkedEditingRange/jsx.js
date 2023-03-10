// @flow

declare function Foo(): React$Node;

{
  <Foo>

  </Foo>
}

{
  <div>
    <Foo>

    </Foo>
  </div>
}

{
  <>

  </>
}

{
  <Foo />
}
