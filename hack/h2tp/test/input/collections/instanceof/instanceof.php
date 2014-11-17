<?hh
namespace Boo {
  interface Foo {}
  function somedata($c) {
    $t = gettype($c);
    if ($t === "object") {
      $t = get_class($c);
    }
    if ($c instanceof KeyedIterable) {
      echo ("$t is a KeyedIterable\n");
    }
    if ($c instanceof Traversable) {
      echo ("$t is a Traversable\n");
    }
    if ($c instanceof \Traversable) {
      echo ("$t is a \Traversable\n");
    }
    if ($c instanceof \HH\Traversable) {
      echo ("$t is a \HH\Traversable\n");
    }
    if ($c instanceof Iterator) {
      echo ("$t is an Iterator\n");
    }
    if ($c instanceof \HH\Iterator) {
      echo ("$t is an \HH\Iterator\n");
    }
    if ($c instanceof \Iterator) {
      echo ("$t is an Iterator\n");
    }
    if ($c instanceof Vector) {
      echo ("$t is a Vector\n");
    }
    $y = "HH\Traversable";
    if ($c instanceof $y) {
      echo ("$t is a \"HH\Traversable\"\n");
    }
    if ($c instanceof Foo) {
      echo ("$t is a Foo\n");
    }
    $y = "\Boo\Foo";
    if ($c instanceof $y) {
      echo ("$t is a \"\Boo\Foo\"\n");
    }
    echo ("\n\n");
  }
  class Bar implements Foo {
  }
  somedata(Vector {"c`est", "la", "vie"});
  somedata((Vector {})->getIterator());
  somedata(array("so", "it", "goes"));
  somedata(new Bar());
}
