// @flow

const o = {
    f: 1,
    g: "",
};

const {
    f,
    ...r
} = o;

export default r;
