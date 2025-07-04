{
    let x = function (y: number) { return y; };
    if (x) { } // error, x is truthy
    if (x(4)) {} // ok, x is called

    let y = (a:number) => a/2;
    if (y) { } // error, y is truthy
    if (y(4)) {} // ok, y is called

    let z = () => "";
    if (z) { } // error, z is truthy
    if (z()) {} // ok, z is called
}
