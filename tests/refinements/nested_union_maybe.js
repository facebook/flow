declare function foo(x : number) : ?string;

declare var x : number;
const y = true
    ? foo // error: foo is always truthy
        ? foo(x)
        : null
    : 'fail';


if(y != null) { (y : string) }
