// @flow

export var emptyObj = {};

export var singleProp = {p1: 42};
export var multiProp = {p1: 42, p2: 42};
export var nestedObject = {p1: {p2: 42}};

export var dict: {[key: string]: string} = {};
export var dictWithProps: {
  p1: string,
  [key: string]: number,
} = {p1: "asdf"};
