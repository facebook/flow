<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function print_res($name, $c) {
  echo
    ("$name : ".(\hacklib_cast_as_boolean($c) ? " TRUE " : " FALSE ")."\n")
  ;
}
$o1 = 10;
$o2 = "10";
print_res("straightforward equality", 10 == "10");
print_res("simple equality", \hacklib_equals($o1, $o2));
print_res("simple inequality", \hacklib_not_equals($o1, $o2));
print_res(
  "vec and immvec",
  \hacklib_equals(new \HH\Vector(array()), new \HH\ImmVector(array()))
);
$m1 = \HH\Map::hacklib_new(array("t", 1), array("tt", 2));
$m2 = \HH\Map::hacklib_new(array(1, "t"), array(2, "tt"));
print_res("map key order", \hacklib_equals($m1, $m2));
$p1 = \HH\Pair::hacklib_new(1, 2);
$p2 = \HH\Pair::hacklib_new(1, 2);
$p1->getIterator();
print_res("collections with additional state", \hacklib_equals($p1, $p2));
print_res(
  "Pairs can be unequal",
  \hacklib_not_equals(
    \HH\Pair::hacklib_new("high", "tea"),
    \HH\Pair::hacklib_new("high", "tee")
  )
);
print_res(
  "Vector uses ==",
  \hacklib_equals(new \HH\Vector(array(1)), new \HH\ImmVector(array("1")))
);
print_res(
  "Even if Empty",
  \hacklib_equals(new \HH\Vector(array()), new \HH\ImmVector(array()))
);
print_res(
  "Number of Items",
  \hacklib_equals(new \HH\Set(array(1)), new \HH\Set(array(1, 2)))
);
print_res(
  "Sets use === on keys",
  \hacklib_equals(new \HH\Set(array(1)), new \HH\Set(array("1")))
);
print_res(
  "Sets can equal ImmSets",
  \hacklib_equals(new \HH\Set(array("a", 1)), new \HH\ImmSet(array(1, "a")))
);
print_res(
  "Maps do both",
  \hacklib_equals(
    \HH\Map::hacklib_new(array("1", 1), array("zork", "mindy")),
    \HH\Map::hacklib_new(array(1, "1"), array("mindy", "zork"))
  )
);
print_res(
  "Maps can equal ImmMaps",
  \hacklib_equals(
    \HH\ImmMap::hacklib_new(array("o", 1), array("mork", 1)),
    \HH\Map::hacklib_new(array(1, "o"), array(1, "mork"))
  )
);
print_res("Check for false", \hacklib_equals(new \HH\Vector(array()), false));
print_res("Check for true", \hacklib_equals(true, new \HH\Vector(array())));
print_res(
  "Using not equals",
  \hacklib_not_equals(\HH\Map::hacklib_new(array(), array()), true)
);
print_res(
  "Nested Comparison True",
  \hacklib_equals(
    new \HH\Vector(
      array(
        \HH\Map::hacklib_new(array(1), array("zoom")),
        new \HH\Vector(array("33"))
      )
    ),
    new \HH\Vector(
      array(
        \HH\Map::hacklib_new(array(1), array("zoom")),
        new \HH\ImmVector(array(33))
      )
    )
  )
);
print_res(
  "Nested Comparison False",
  \hacklib_equals(
    new \HH\Vector(
      array(
        \HH\Map::hacklib_new(array(1), array("zoom")),
        new \HH\Set(array("33"))
      )
    ),
    new \HH\Vector(
      array(
        \HH\Map::hacklib_new(array(1), array("zoom")),
        new \HH\Set(array(33))
      )
    )
  )
);
print_res(
  "Compare with null",
  \hacklib_equals(null, new \HH\Vector(array()))
);
print_res(
  "Compare values that are null",
  \hacklib_equals(new \HH\Vector(array(false)), new \HH\Vector(array(null)))
);
