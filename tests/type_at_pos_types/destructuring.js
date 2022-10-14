// @flow

let [x, y] = [1, 2];

export const X = {
  returnsATuple: function(): [number, number] {
    return [1, 2];
  },

  test: function() {
    let [a, b] = X.returnsATuple();
  }
};
