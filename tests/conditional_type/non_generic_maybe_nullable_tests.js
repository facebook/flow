type MaybeStringExtendsString = (?string) extends string ? true : false;
true as MaybeStringExtendsString; // error
false as MaybeStringExtendsString; // ok

function optionalTypeTest(x?: string) {
  type OptionalStringExtendsString = (typeof x) extends string ? true : false;
  true as OptionalStringExtendsString; // error
  false as OptionalStringExtendsString; // ok
}

type StringExtendsMaybeString = string extends (?string) ? true : false;
true as StringExtendsMaybeString; // ok
false as StringExtendsMaybeString; // error

type StringOrNullOrVoidExtendsString = (string | null | void) extends string ? true : false;
true as StringOrNullOrVoidExtendsString; // error
false as StringOrNullOrVoidExtendsString; // ok

type StringExtendsStringOrNullOrVoid = string extends (string | null | void) ? true : false;
true as StringExtendsStringOrNullOrVoid; // ok
false as StringExtendsStringOrNullOrVoid; // error
