// A later libdef namespace declaration can read type members from the
// already-merged namespace without qualifying them.
declare namespace LibMulti {
  type Shared = string;
}

declare namespace LibMulti {
  type Alias = Shared;
}

declare namespace LibOther {
  type Shared = number;
}

declare namespace LibOther {
  type Alias = Shared;
}

declare namespace LibSameBody {
  type Alias = Shared;
  type Shared = boolean;
}

declare namespace LibForward {
  type Alias = Later;
}

declare namespace LibForward {
  type Later = string;
}

declare namespace LibOrder {
  type Shared = "outer";
  declare namespace Inner {
    type Shared = "inner";
  }
}

declare namespace LibOrder {
  declare namespace Inner {
    declare namespace Leaf {
      declare const nearest: Shared;
    }
  }
}

declare namespace LibValueOrder {
  declare const SharedValue: "outer";
  declare namespace Inner {
    declare const SharedValue: "inner";
  }
}

declare namespace LibValueOrder {
  declare namespace Inner {
    declare namespace Leaf {
      declare const nearestValue: typeof SharedValue;
    }
  }
}

declare namespace LibNestedMerge {
  declare namespace Inner {
    type Shared = "nested";
    declare const SharedValue: "nested value";
  }
}

declare namespace LibNestedMerge {
  declare namespace Inner {
    type Alias = Shared;
    declare const value: Alias;
    declare const valueFromTypeof: typeof SharedValue;
  }
}
