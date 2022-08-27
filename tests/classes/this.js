// @flow

class Foo {
  f : string

  m() {
    let x = this.f;
    if (x === '') {
      let x1 = { x : "" };
    }
  }
}

declare class Bar<T> {
    state : T,
    setState($Shape<T>): void;
}

class Baz extends Bar<any> {
  state: {z:{x:number,y:number}} = {
    z: {
      x: 0,
      y: 0,
    },
  };

  foo() {
    declare var z :  { +x: number };
    this.setState({ z });
  }
}
