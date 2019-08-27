// @flow

const ENUM = Object.freeze({
    THING: 'THING',
    THING_TWO: 'THING_TWO',
});

const notAThing: typeof ENUM.THING = 'NOT_A_THING' // error

const isAThing: $Values<typeof ENUM> = 'IS_A_THING' // error
