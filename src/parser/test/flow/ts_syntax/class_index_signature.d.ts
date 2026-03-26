// Index signature in class body in ambient context
class MyClass {
  [key: string]: any;
  [index: number]: string;
  static [key: string]: number;
  readonly [key: string]: boolean;
  [key: string]?: string;
}
