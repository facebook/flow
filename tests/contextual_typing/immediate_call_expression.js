// @flow

const x: number => void = (function () {
    return (x) => {
        (x: number); // okay
        (x: string); // error number ~> string
    };
})();
