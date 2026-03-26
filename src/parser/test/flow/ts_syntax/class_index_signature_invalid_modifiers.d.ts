// Invalid modifiers on class index signatures
class C {
  declare [key: string]: any;
  public [key: string]: any;
  abstract [key: string]: any;
}
