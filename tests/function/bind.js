let tests = [
  function(x: (a: string, b: string) => void) {
    let y = x.bind(x, 'foo');
    y('bar'); // ok
    y(123); // error, number !~> string
  },

  // callable objects
  function(x: { (a: string, b: string): void, ... }) {
    let y = x.bind(x, 'foo');
    y('bar'); // ok
    y(123); // error, number !~> string
  },

  // non-callable objects
  function(x: { a: string, ... }) {
    x.bind(x, 'foo'); // error
  },

  // callable objects with overridden `bind` method
  function(x: {(a: string, b: string): void, bind(a: string): void, ...}) {
    x.bind('foo') as void; // ok
    x.bind(123) as void; // error, number !~> string
  },

];

class A {}

A.bind(3);

class Receiver {
  method(x: string, y: number): boolean {
    return y > 0;
  }
}

const receiver = new Receiver();
const bound = receiver.method.bind(receiver, 'prefix');
bound(1) as boolean;
bound('bad'); // ERROR
receiver.method.bind(receiver, 1); // ERROR

const withoutArgs = receiver.method.bind(receiver);
withoutArgs('prefix', 1) as boolean;

const withSpread = receiver.method.bind(receiver, ...(['prefix'] as [string]));
withSpread(1) as boolean;
withSpread('bad'); // ERROR
