/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const Enum = require('../index');

describe('Enum', () => {
  const E = Enum({A: 1, B: 2});

  test('access members', () => {
    expect(E.A).toBe(1);
    expect(E.B).toBe(2);
  });

  test('only two own properties', () => {
    expect(Object.getOwnPropertyNames(E).length).toBe(2);
  });

  test('no enumerable properties', () => {
    let count = 0;
    for (const _ in E) {
      count++;
    }
    expect(count).toBe(0);
  });

  test('not extensible', () => {
    expect(Object.isExtensible(E)).toBe(false);
    expect(() => {
      E.C = 3;
    }).toThrow();
    expect(E.C).toBe(undefined);
    expect(Object.getOwnPropertyNames(E).length).toBe(2);
  });

  test('not writable', () => {
    expect(() => {
      E.A = 66;
    }).toThrow();
    expect(() => {
      E.B = 66;
    }).toThrow();
    expect(E.A).toBe(1);
    expect(E.B).toBe(2);
  });

  test('not deletable', () => {
    expect(() => {
      delete E.A;
    }).toThrow();
    expect(() => {
      delete E.B;
    }).toThrow();
    expect(E.A).toBe(1);
    expect(E.B).toBe(2);
  });

  test('not configurable', () => {
    expect(() => {
      Object.defineProperty(E, 'A', {
        value: 66,
        writable: true,
        enumerable: true,
        configurable: true,
      });
    }).toThrow('Cannot redefine property: A');
  });

  test('not castable to primitive', () => {
    expect(() => {
      '' + E;
    }).toThrow('Cannot convert object to primitive value');
  });

  test('prototype frozen', () => {
    const F = Enum({A: 1});
    expect(() => {
      Object.getPrototypeOf(F).isValid = () => true;
    }).toThrow();
    expect(F.isValid(1)).toBe(true);
  });

  describe('prototype methods', () => {
    test('isValid', () => {
      expect(E.isValid(1)).toBe(true);
      expect(E.isValid(2)).toBe(true);
      expect(E.isValid(3)).toBe(false);
      expect(E.isValid(null)).toBe(false);
      expect(E.isValid(undefined)).toBe(false);
    });

    test('cast', () => {
      expect(E.cast(1)).toBe(E.A);
      expect(E.cast(2)).toBe(E.B);
      expect(E.cast(3)).toBe(undefined);
    });

    test('members', () => {
      expect(Array.from(E.members())).toEqual([1, 2]);
    });
  });
});

describe('Enum.Mirrored', () => {
  const E = Enum.Mirrored(['A', 'B']);

  test('access members', () => {
    expect(E.A).toBe('A');
    expect(E.B).toBe('B');
  });

  test('only two own properties', () => {
    expect(Object.getOwnPropertyNames(E).length).toBe(2);
  });

  test('no enumerable properties', () => {
    let count = 0;
    for (const _ in E) {
      count++;
    }
    expect(count).toBe(0);
  });

  test('not extensible', () => {
    expect(Object.isExtensible(E)).toBe(false);
    expect(() => {
      E.C = 'C';
    }).toThrow();
    expect(E.C).toBe(undefined);
    expect(Object.getOwnPropertyNames(E).length).toBe(2);
  });

  test('not writable', () => {
    expect(() => {
      E.A = 'foo';
    }).toThrow();
    expect(() => {
      E.B = 'bar';
    }).toThrow();
    expect(E.A).toBe('A');
    expect(E.B).toBe('B');
  });

  test('not deletable', () => {
    expect(() => {
      delete E.A;
    }).toThrow();
    expect(() => {
      delete E.B;
    }).toThrow();
    expect(E.A).toBe('A');
    expect(E.B).toBe('B');
  });

  test('not configurable', () => {
    expect(() => {
      Object.defineProperty(E, 'A', {
        value: 'x',
        writable: true,
        enumerable: true,
        configurable: true,
      });
    }).toThrow('Cannot redefine property: A');
  });

  test('not castable to primitive', () => {
    expect(() => {
      '' + E;
    }).toThrow('Cannot convert object to primitive value');
  });

  test('prototype frozen', () => {
    const F = Enum.Mirrored(['A']);
    expect(() => {
      Object.getPrototypeOf(F).isValid = () => true;
    }).toThrow();
    expect(F.isValid('A')).toBe(true);
  });

  describe('prototype methods', () => {
    test('isValid', () => {
      expect(E.isValid('A')).toBe(true);
      expect(E.isValid('B')).toBe(true);
      expect(E.isValid('C')).toBe(false);
      expect(E.isValid(null)).toBe(false);
      expect(E.isValid(undefined)).toBe(false);

      const s = {
        toString() {
          return 'A';
        },
      };
      expect(E.isValid(s)).toBe(false);
    });

    test('cast', () => {
      expect(E.cast('A')).toBe(E.A);
      expect(E.cast('B')).toBe(E.B);
      expect(E.cast('C')).toBe(undefined);
    });

    test('members', () => {
      expect(Array.from(E.members())).toEqual(['A', 'B']);
    });
  });
});
