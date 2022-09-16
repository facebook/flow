// @flow

const x: number => void = (function () {
    return (x) => {}; // no annotation needed
})();

const y: number => void = (function (y: number) {
    return (x) => {}; // error missing annotation
})(1);
