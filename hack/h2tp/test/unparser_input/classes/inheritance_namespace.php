<?hh
namespace foo {
  class A {
    public function a1($x) {
      return $x*$x;
    }
  }
}
namespace bar\inner {
  class B extends \foo\A {
    public function a2($x) {
      return $this->a1($x*2);
    }
  }
}
namespace bar {
  function flub() {
    $b = new namespace\inner\B();
    echo($b->a2(10)."\n");
  }
}

namespace {
  \bar\flub();
}
