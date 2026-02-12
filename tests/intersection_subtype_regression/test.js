type NonSemanticColor =
  | 'gray'
  | 'blue';

type IconColor =
  | 'active'
  | 'button'
  | NonSemanticColor;

type MediaColor = 'default' | NonSemanticColor;

{
  declare const x: IconColor & MediaColor;
  x as MediaColor; // OK: (IconColor & MediaColor) <: MediaColor
}
{
  declare const x: IconColor & MediaColor;
  x as IconColor; // OK: (IconColor & MediaColor) <: IconColor
}
{
  declare const x: IconColor & MediaColor;
  x as MediaColor; // OK
  x as IconColor;  // OK
}
