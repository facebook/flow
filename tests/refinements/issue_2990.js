/* @flow */
type exactObjectType =
  | {|
      "a.b": {}
    |}
  | {|
      "a.c": string
    |};

type disjoint =
  | {
      "tag.tag": "first",
      "a.b": {}
    }
  | {
      "tag.tag": "second",
      "a.c": string
    };

const testDisjoint = (param: disjoint) => {
  if (param["tag.tag"] === "second") {
    (param["tag.tag"]: "second"); // ok
    (param["tag.tag"]: "first"); // error
  } else {
    (param["tag.tag"]: "second"); // ok
    (param["tag.tag"]: "first"); // error
  }
};

const testFunction = (param: exactObjectType) => {
  if (param["a.c"] != null) {
    (param["a.c"]: string); // ok
    (param["a.c"]: void); // error
  } else {
    (param["a.c"]: string); // error
    (param["a.c"]: void); // ok
  }
};
