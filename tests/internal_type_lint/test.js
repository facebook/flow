enum E of string {}

type T1_bad = $EnumValue<string>; // error
type T1_good = EnumValue<string>; // ok
type T2_bad = $Enum<E>; // error
type T2_good = Enum<E>; // ok

type C = component();

type T3_bad = React$ElementRef<C>; // error
type T3_good = React.ElementRef<C>; // ok
type T4_bad = React$ElementConfig<C>; // error
type T4_good = React.ElementConfig<C>; // ok
type T5_bad = React$ElementProps<C>; // error
type T5_good = React.ElementProps<C>; // ok
type T6_bad = $ReactDeepReadOnly<{}>; // error
