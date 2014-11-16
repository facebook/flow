<?hh
class A {
  public function me() {
    return get_class($this);
  }
}
class B extends A {}
class Foo<+T as A> {
  public function __construct(private T $t) {
  }
  public function getMe() {
    return $this->t->me();
  }
}
function printAFoo(Foo<A> $f) {
  echo($f->getMe()."\n");
}
function callWithFooA(Foo<A> $f) {
  printAFoo($f);
}
function callWithFooB(Foo<B> $f) {
  printAFoo($f);
}
callWithFooA(new Foo(new A()));
callWithFooB(new Foo(new B()));
interface Bar<T> {
  public function gimme(): T;
}
trait Baz<T> {
  public function gimme() {
    throw new Exception("cheat");
  }
}
class DunDunDun<T as A> extends Foo<T> implements Bar<T> {
  use Baz<T>;
}
callWithFooA(new DunDunDun(new A()));
