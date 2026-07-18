'' as LibMulti.Alias; // ok
1 as LibMulti.Alias; // err: number ~> string

1 as LibOther.Alias; // ok
'' as LibOther.Alias; // err: string ~> number

true as LibSameBody.Alias; // ok
1 as LibSameBody.Alias; // err: number ~> boolean

'' as LibForward.Alias; // ok
1 as LibForward.Alias; // err: number ~> string

LibOrder.Inner.Leaf.nearest as "inner"; // ok
LibOrder.Inner.Leaf.nearest as "outer"; // err: "inner" ~> "outer"

LibValueOrder.Inner.Leaf.nearestValue as "inner"; // ok
LibValueOrder.Inner.Leaf.nearestValue as "outer"; // err: "inner" ~> "outer"

LibNestedMerge.Inner.value as "nested"; // ok
LibNestedMerge.Inner.value as "other"; // err: "nested" ~> "other"
LibNestedMerge.Inner.valueFromTypeof as "nested value"; // ok
LibNestedMerge.Inner.valueFromTypeof as "other"; // err: "nested value" ~> "other"
