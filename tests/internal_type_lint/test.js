enum E of string {}

type T1_bad = $EnumValue<string>; // error
type T1_good = EnumValue<string>; // ok
type T2_bad = $Enum<E>; // error
type T2_good = Enum<E>; // ok

type C = component();

type T3_good = React.ElementRef<C>; // ok
type T4_bad = React$ElementConfig<C>; // error
type T4_good = React.ElementConfig<C>; // ok
type T5_bad = $ReactDeepReadOnly<{}>; // error
type T6_bad = React$Node; // error
type T6_good = React.Node; // ok

type T7_bad = $Omit<{}, empty>; // error
type T7_good = Omit<{}, empty>; // ok
