/* @flow */
const o = {
    A: require('./A'),
    ...require('./B'),
};
module.exports = o;
