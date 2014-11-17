<?hh
namespace foo {
  class Foo {}
  new Vector();
  new ImmVector(array());
  new Map();
  new ImmMap(Vector {});
  new Set();
  new ImmSet(null);
  new Foo();
}
namespace {
  new \HH\Vector();
  new \HH\ImmVector(array());
  new \HH\Map();
  new \HH\ImmMap(Vector {});
  new \HH\Set();
  new \HH\ImmSet(null);
  new \foo\Foo();
}
