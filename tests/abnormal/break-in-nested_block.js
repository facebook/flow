// @flow

switch ('foo') {
    case 'foo': {
        const foo = 3;
        break;
    }
}

const {foo} = {foo:3};

for (;;) {
    const bar = 3;
    break;
}

const {bar} = {bar:3};
