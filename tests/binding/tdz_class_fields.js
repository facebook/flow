/** @flow */

class Clazz {
    a: number = forwardReferenced; // No error
    static b: number = forwardReferenced; // Should error
    #c: number = forwardReferenced; // No error
    static #d: number = forwardReferenced; // Should error
}

const forwardReferenced = 0;
