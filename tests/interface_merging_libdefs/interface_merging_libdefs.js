// Basic property merging: both `a` and `b` should be visible
declare var merged_props: MergedProps;
merged_props.a as string; // ok
merged_props.b as number; // ok
merged_props.a as number; // error: string ~> number
merged_props.b as string; // error: number ~> string

// Method overload merging: both foo signatures should exist, bar from second interface
declare var merged_methods: MergedMethods;
merged_methods.foo("") as string; // ok
merged_methods.foo(0) as number; // ok
merged_methods.bar() as boolean; // ok

// Extends merging: both Base1 and Base2 should be extended
declare var merged_extends: MergedExtends;
merged_extends.a as string; // ok
merged_extends.b as number; // ok
merged_extends.x as string; // ok (from Base1)
merged_extends.y as number; // ok (from Base2)

// Three-way merge
declare var three_way: ThreeWay;
three_way.first as string; // ok
three_way.second as number; // ok
three_way.third as boolean; // ok
three_way.first as number; // error: string ~> number
