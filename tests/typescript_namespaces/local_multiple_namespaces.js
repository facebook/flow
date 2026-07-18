declare namespace LocalMulti {
  type Shared = string;
}

declare namespace LocalMulti {
  declare const value: Shared;
}

LocalMulti.value as string; // ok
LocalMulti.value as number; // err: string ~> number

declare namespace LocalOther {
  type Shared = number;
}

declare namespace LocalOther {
  declare const value: Shared;
}

LocalOther.value as number; // ok
LocalOther.value as string; // err: number ~> string

declare namespace LocalSameBody {
  type Alias = Shared;
  type Shared = boolean;
}

true as LocalSameBody.Alias; // ok
1 as LocalSameBody.Alias; // err: number ~> boolean

declare namespace LocalForward {
  type Alias = Later;
}

declare namespace LocalForward {
  type Later = string;
}

'' as LocalForward.Alias; // ok
1 as LocalForward.Alias; // err: number ~> string

declare namespace LocalValueForward {
  type Alias = typeof later;
}

declare namespace LocalValueForward {
  declare const later: "later value";
}

"later value" as LocalValueForward.Alias; // ok
"other" as LocalValueForward.Alias; // err: "other" ~> "later value"

declare namespace LocalNestedForward {
  declare namespace Inner {
    type Alias = Later;
  }
}

declare namespace LocalNestedForward {
  declare namespace Inner {
    type Later = "nested forward";
  }
}

"nested forward" as LocalNestedForward.Inner.Alias; // ok
"other" as LocalNestedForward.Inner.Alias; // err: "other" ~> "nested forward"

declare const LocalSharedValue: "outer";

declare namespace LocalOrder {
  type Shared = "outer";
  declare namespace Inner {
    type Shared = "inner";
  }
}

declare namespace LocalOrder {
  declare namespace Inner {
    declare const LocalSharedValue: "inner";
    declare namespace Leaf {
      declare const nearest: Shared;
      declare const nearestValue: typeof LocalSharedValue;
    }
  }
}

LocalOrder.Inner.Leaf.nearest as "inner"; // ok
LocalOrder.Inner.Leaf.nearest as "outer"; // err: "inner" ~> "outer"
LocalOrder.Inner.Leaf.nearestValue as "inner"; // ok
LocalOrder.Inner.Leaf.nearestValue as "outer"; // err: "inner" ~> "outer"

declare namespace LocalNestedMerge {
  declare namespace Inner {
    type Shared = "nested";
  }
}

declare namespace LocalNestedMerge {
  declare namespace Inner {
    type Alias = Shared;
    declare const value: Alias;
  }
}

LocalNestedMerge.Inner.value as "nested"; // ok
LocalNestedMerge.Inner.value as "other"; // err: "nested" ~> "other"
