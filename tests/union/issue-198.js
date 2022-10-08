var p = new Promise<number>(function(resolve, reject) {
    resolve(5);
})
    .then(function(num) {
        return num.toFixed();
    })
    .then(function(str) {
        // This should fail because str is string, not number
        return str.toFixed();
    });
