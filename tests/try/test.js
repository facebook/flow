function foo() {
    var x = 0;
    var y;
    try {
        x = "";
    } catch(e) {
        x = false;
        throw -1;
    } finally {
        y = {};
    }
    x(); // x is neither a number nor a boolean
    y(); // y is not undefined
}

function bar(response) {
    var payload;
    try {
        payload = JSON.parse(response);
    } catch (e) {
        throw new Error('...');
    }
    if (payload.error) {
        // ...
    }
}

function qux() {
    var x = 5;
    try {
        throw -1;
    } finally {
    }
    x(); // unreachable
}
