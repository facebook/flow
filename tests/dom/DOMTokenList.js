// @flow

let tests = [
  //length
  function (element: DOMTokenList) {
    element.length as number;
  },

  //item
  function (element: DOMTokenList) {
    element.item(1) as string;
  },

  //contains
  function (element: DOMTokenList) {
    element.contains('test-token') as boolean;
  },

  //add
  function (element: DOMTokenList) {
    element.add('test-class');
    element.add('class-one', 'class-two');
  },

  //remove
  function (element: DOMTokenList) {
    element.remove('token') as void;
    element.remove('token', 'token') as void;
  },

  //toggle
  function (element: DOMTokenList) {
    element.toggle('test-token') as boolean;
    element.toggle('test-token', true) as boolean;
  },

  //replace
  function (element: DOMTokenList) {
    element.replace('old-token', 'new-token') as boolean;
  },

  //forEach
  function (element: DOMTokenList) {
    element.forEach((value, index, list) => {
      const testValue: string = value;
      const testIndex: number = index;
      const testList: DOMTokenList = list;
    }) as void;
  },

  //entries
  function (element: DOMTokenList) {
    element.entries() as Iterator<[number, string]>;
  },

  //keys
  function (element: DOMTokenList) {
    element.keys() as Iterator<number>;
  },

  //values
  function (element: DOMTokenList) {
    element.values() as Iterator<string>;
  },
];
