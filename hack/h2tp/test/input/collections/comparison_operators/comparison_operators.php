<?hh
function print_res($name, $c) {
  echo ("$name : ".($c ? " TRUE " : " FALSE ")."\n");
}
$o1 = 10;
$o2 = "10";
print_res("straightforward equality", 10 == "10");
print_res("simple equality", $o1 == $o2);
print_res("simple inequality", $o1 != $o2);
print_res("vec and immvec", Vector {} == ImmVector {});
$m1 = Map {"t" => "tt", 1 => 2};
$m2 = Map {1 => 2, "t" => "tt"};
print_res("map key order", $m1 == $m2);
$p1 = Pair {1, 2};
$p2 = Pair {1, 2};
$p1->getIterator();
print_res("collections with additional state", $p1 == $p2);
print_res("Pairs can be unequal", Pair {"high", "tea"} != Pair {"high", "tee"});
print_res("Vector uses ==", Vector {1} == ImmVector {"1"});
print_res("Even if Empty", Vector {} == ImmVector {});
print_res("Number of Items", Set {1} == Set {1, 2});
print_res("Sets use === on keys", Set {1} == Set {"1"});
print_res("Sets can equal ImmSets", Set {"a", 1} == ImmSet {1, "a"});
print_res("Maps do both", Map {"1" => "zork", 1 => "mindy"} == Map {1 => "mindy", "1" => "zork"});
print_res("Maps can equal ImmMaps", ImmMap {"o" => "mork", 1 => 1} == Map {1 => 1, "o" => "mork"});
print_res("Check for false", Vector {} == false);
print_res("Check for true", true == Vector {});
print_res("Using not equals", Map {} != true);
print_res("Nested Comparison True", Vector {Map {1 => "zoom"}, Vector {"33"}} == Vector {Map {1 => "zoom"}, ImmVector {33}});
print_res("Nested Comparison False", Vector {Map {1 => "zoom"}, Set {"33"}} == Vector {Map {1 => "zoom"}, Set {33}});
print_res("Compare with null", null == Vector {});
print_res("Compare values that are null", Vector {false} == Vector {null});
