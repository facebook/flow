/** @flow */

class Clazz {
    a = forwardReferenced; // No error
    static b = forwardReferenced; // Should error
    #c = forwardReferenced; // No error
    static #d = forwardReferenced; // Should error
}

const forwardReferenced = 0;
