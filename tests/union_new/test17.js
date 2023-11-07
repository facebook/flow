// @flow

// Array#concat

([] as Array<empty>).concat([]);

([]: Array<empty>).concat([0, 1])[1] as string;
