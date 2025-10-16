{
   type $NonMaybeType = string | number;
   type NonNullable = number;
   type $ReadOnly = string;
   type Readonly = string;
   type $Keys = string;
   type $Values<T> = Array<T>;
   type Values = Array<string>;
}
{
   declare type $NonMaybeType = string | number;
   declare type NonNullable = number;
   declare type $ReadOnly = string;
   declare type Readonly = string;
   declare type $Keys = string;
   declare type $Values<T> = Array<T>;
   declare type Values = Array<string>;
}

{
   declare class $NonMaybeType {}
   declare class NonNullable {}
   declare class $ReadOnly {}
   declare class Readonly {}
   declare class $Keys<TT> {}
   declare class $Values<T> {}
   declare class Values {}
}

{
   class $NonMaybeType {}
   class NonNullable {}
   class $ReadOnly {}
   class Readonly {}
   class $Keys<TT> {}
   class $Values<T> {}
   class Values {}
}
