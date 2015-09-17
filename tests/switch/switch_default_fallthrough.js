/**
 * @flow
 */
function foo(x : mixed): string {
    var a = "";
    var b = "";
    switch (x) {
      case "foo":
        a = 0;
      default:
        b = 0;
    }
    (a : string);
    return b;
}

function baz(x: mixed): number {
    var a = "";
    var b = "";

    switch (x) {
      case "baz":
        a = 0;
        break;
      case "bar":
        a = "";
      default:
        b = 0;

    }

    (a : string);
    (b : string);

    return a+b;
}
