declare const React: any;
declare function Render(): null;

class StringKeyAssignment {
  value: string;
  constructor() {
    this['value'] = 'hi';
  }
}

class StringKeyRead {
  value: string;
  constructor() {
    this['value']; // error
    this.value = 'hi';
  }
}

class JsxTagRead {
  Render: typeof Render;
  constructor() {
    <this.Render />; // error
    this.Render = Render;
  }
}
