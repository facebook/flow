enum E {A, B}

E.A as $NotNullOrVoid; // OK
E as $NotNullOrVoid; // OK

{
  declare const x: EnumValue<>;
  x as $NotNullOrVoid; // OK
}

{
  declare const x: Enum<>;
  x as $NotNullOrVoid; // OK
}
