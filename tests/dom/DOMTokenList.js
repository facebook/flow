// @flow

let tests = [
  //length
  function(element: DOMTokenList) {
    (element.length: number);
  },

  //item
  function(element: DOMTokenList) {
    (element.item(1): string);
  },

  //contains
  function(element: DOMTokenList) {
    (element.contains('test-token'): boolean);
  },

  //add
  function(element: DOMTokenList) {
    element.add('test-class');
    element.add('class-one', 'class-two');
  },

  //remove
  function(element: DOMTokenList) {
    (element.remove('token'): void);
    (element.remove('token', 'token'): void);
  },

  //toggle
  function(element: DOMTokenList) {
    (element.toggle('test-token'): boolean);
    (element.toggle('test-token', true): boolean);
  },

  //replace
  function(element: DOMTokenList) {
    (element.replace('old-token', 'new-token'): boolean);
  },

  //forEach
  function(element: DOMTokenList) {
    (element.forEach((value, index, list) => {
      const testValue: string = value;
      const testIndex: number = index;
      const testList: DOMTokenList = list;
    }): void);
  },

  //entries
  function(element: DOMTokenList) {
    (element.entries(): Iterator<[number, string]>);
  },

  //keys
  function(element: DOMTokenList) {
    (element.keys(): Iterator<number>);
  },

  //values
  function(element: DOMTokenList) {
    (element.values(): Iterator<string>);
  },
];
