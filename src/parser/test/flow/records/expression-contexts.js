function test() {
  return R {a: 1, b: 2};
}

const x = foo(R {c: 3});

const y = [R {d: 4}, R {e: 5}];

const z = {nested: R {f: 6}};

class C extends not_parsed_as_a_record_expression {}
