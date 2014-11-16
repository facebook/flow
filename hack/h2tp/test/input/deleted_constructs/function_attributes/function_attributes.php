<?hh
  <<__Foobar>>
function foo(string $key): string {
  echo(" looking for $key: \n");
  return $key." - hooah!";
}

class Bar {
  <<__Foobar>>
  public function baz(int $index) {
    echo("getting value at index $index\n");
    return $index ** 3;
  }
}
