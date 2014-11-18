module.exports = {
  'Type Annotations': {
    'function foo(numVal: number, x: number){}': {
      'body.0.params': [
        {
          'name': 'numVal',
          'typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
        },
        {
          'name': 'x',
          'typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
        },
      ]
    },
    'function foo(numVal: number, strVal: string){}': {
      'body.0': {
        'params': {
          '0.typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
          '1.typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation'
        },
        'returnType': null,
        'typeParameters': null,
      }
    },
    'function foo(numVal: number, untypedVal){}': {
      'body.0.params.0.typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
      'body.0.params.1.typeAnnotation': null
    },
    'function foo(untypedVal, numVal: number){}': {
      'body.0.params.0.typeAnnotation': null,
      'body.0.params.1.typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation'
    },
    'function foo(nullableNum: ?number){}': {
      'body.0.params.0.typeAnnotation.typeAnnotation': {
        'type': 'NullableTypeAnnotation',
        'typeAnnotation.type': 'NumberTypeAnnotation'
      }
    },
    'function foo(callback: () => void){}': {
      'body.0.params.0.typeAnnotation.typeAnnotation': {
        'type': 'FunctionTypeAnnotation',
        'params': [],
        'returnType.type': 'VoidTypeAnnotation',
      }
    },
    'function foo(callback: () => number){}': {
      'body.0.params.0.typeAnnotation.typeAnnotation': {
        'type': 'FunctionTypeAnnotation',
        'params': [],
        'returnType.type': 'NumberTypeAnnotation',
      }
    },
    'function foo(callback: (_:bool) => number){}': {
      'body.0.params.0.typeAnnotation.typeAnnotation': {
        'type': 'FunctionTypeAnnotation',
        'params.0': {
          'type': 'FunctionTypeParam',
          'name.name': '_',
          'typeAnnotation.type': 'BooleanTypeAnnotation'
        },
        'returnType.type': 'NumberTypeAnnotation'
      }
    },
    'function foo(callback: (_1:bool, _2:string) => number){}': {
      'body.0.params.0.typeAnnotation.typeAnnotation': {
        'type': 'FunctionTypeAnnotation',
        'params': [
          {
            'name.name': '_1',
            'typeAnnotation.type': 'BooleanTypeAnnotation'
          },
          {
            'name.name': '_2',
            'typeAnnotation.type': 'StringTypeAnnotation'
          },
        ],
        'returnType.type': 'NumberTypeAnnotation'
      }
    },
    'function foo():number{}': {
      'body.0.returnType.typeAnnotation.type': 'NumberTypeAnnotation',
    },
    'function foo():() => void{}': {
      'body.0.returnType.typeAnnotation': {
        'type': "FunctionTypeAnnotation",
        'params': [],
        'returnType.type': 'VoidTypeAnnotation',
      }
    },
    'function foo():(_:bool) => number{}': {
      'body.0.returnType.typeAnnotation': {
        'type': 'FunctionTypeAnnotation',
        'params.0': {
          'type': 'FunctionTypeParam',
          'name.name': '_',
          'typeAnnotation.type': 'BooleanTypeAnnotation'
        },
        'returnType.type': 'NumberTypeAnnotation'
      }
    },
    'function foo(): {} {}': {
      'body.0.returnType.typeAnnotation.type': 'ObjectTypeAnnotation',
    },
    'function foo<T>() {}': {
      'body.0.typeParameters.params': [
        {
          'type': 'Identifier',
          'name': 'T',
        }
      ]
    },
    'function foo<T,S>() {}': {
      'body.0.typeParameters.params': [
        {
          'type': 'Identifier',
          'name': 'T',
        },
        {
          'type': 'Identifier',
          'name': 'S',
        }
      ]
    },
    'function foo(...typedRest: Array<number>){}': {
      'body.0.rest': {
        'name': 'typedRest',
        'typeAnnotation.typeAnnotation': {
          'type': 'GenericTypeAnnotation',
          'id.name': 'Array',
          'typeParameters.params': [
            {
              'type': 'NumberTypeAnnotation',
            }
          ],
        }
      }
    },
    'a=function<T,S>() {}': {
      'body.0.expression.right.typeParameters.params': [
        {
          'type': 'Identifier',
          'name': 'T',
        },
        {
          'type': 'Identifier',
          'name': 'S',
        }
      ]
    },
    'a={set fooProp(value:number){}}': {
      'body.0.expression.right.properties.0.value.params.0': {
        'name': 'value',
        'typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
      }
    },
    'var numVal:number;': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
    },
    'var numVal:number = otherNumVal;': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
    },
    'var a: ?{numVal: number};': {
      'body.0.declarations.0.id': {
        'typeAnnotation.typeAnnotation': {
          'type': 'NullableTypeAnnotation',
          'typeAnnotation': {
            'type': 'ObjectTypeAnnotation',
            'properties': [
              {
                'key.name': 'numVal',
                'value.type': 'NumberTypeAnnotation',
              }
            ]
          }
        }
      }
    },
    'var a: {numVal: number};': {
      'body.0.declarations.0.id': {
        'typeAnnotation.typeAnnotation': {
          'type': 'ObjectTypeAnnotation',
          'properties': [
            {
              'key.name': 'numVal',
              'value.type': 'NumberTypeAnnotation',
            },
          ],
          'indexers': []
        }
      }
    },
    'var a: {numVal: number; [index: number]: string};': {
      'body.0.declarations.0.id': {
        'typeAnnotation.typeAnnotation': {
          'type': 'ObjectTypeAnnotation',
          'properties': [
            {
              'key.name': 'numVal',
              'value.type': 'NumberTypeAnnotation',
            },
          ],
          'indexers': [{
            'type': 'ObjectTypeIndexer',
            'id.name': 'index',
            'key.type': 'NumberTypeAnnotation',
            'value.type': 'StringTypeAnnotation',
            'loc.start.column': 24,
            'loc.end.column': 47,
          }]
        }
      }
    },
    'var a: {[index: number]: string; [index2: string]: number};': {
      'body.0.declarations.0.id': {
        'typeAnnotation.typeAnnotation': {
          'type': 'ObjectTypeAnnotation',
          'properties': [],
          'indexers': [
            {
              'type': 'ObjectTypeIndexer',
              'id.name': 'index',
              'key.type': 'NumberTypeAnnotation',
              'value.type': 'StringTypeAnnotation',
              'loc.start.column': 8,
              'loc.end.column': 31,
            },
            {
              'type': 'ObjectTypeIndexer',
              'id.name': 'index2',
              'key.type': 'StringTypeAnnotation',
              'value.type': 'NumberTypeAnnotation',
              'loc.start.column': 33,
              'loc.end.column': 57,
            },
          ]
        }
      }
    },
    'var a: {subObj: {strVal: string}}': {
      'body.0.declarations.0.id': {
        'typeAnnotation.typeAnnotation': {
          'type': 'ObjectTypeAnnotation',
          'properties': [
            {
              'key.name': 'subObj',
              'value': {
                'type': 'ObjectTypeAnnotation',
                'properties': [
                  {
                    'key.name': 'strVal',
                    'value.type': 'StringTypeAnnotation',
                  }
                ]
              }
            }
          ]
        }
      }
    },
    'var a: {param1: number; param2: string}': {
      'body.0.declarations.0.id': {
        'typeAnnotation.typeAnnotation': {
          'type': 'ObjectTypeAnnotation',
          'properties': [
            {
              'key.name': 'param1',
              'value.type': 'NumberTypeAnnotation',
            },
            {
              'key.name': 'param2',
              'value.type': 'StringTypeAnnotation',
            }
          ]
        }
      }
    },
    'var a: {param1: number; param2?: string}': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'properties.0.optional': false,
        'properties.1.optional': true,
      }
    },
    'var a: { add(x: number, y:number): number; }': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation.properties': [
        {
          'key.name': 'add',
          'value': {
            'type': 'FunctionTypeAnnotation',
            'params': [
              {
                'name.name': 'x',
                'typeAnnotation.type': 'NumberTypeAnnotation',
              },
              {
                'name.name': 'y',
                'typeAnnotation.type': 'NumberTypeAnnotation',
              }
            ],
            'returnType.type': 'NumberTypeAnnotation',
            'rest': null,
          },
        }
      ]
    },
    'var a: { foo<T>(x: T): number; }': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation.properties': [
        {
          'key.name': 'foo',
          'value': {
            'type': 'FunctionTypeAnnotation',
            'params': [
              {
                'name.name': 'x',
                'typeAnnotation':
                  {
                    'type': 'GenericTypeAnnotation',
                    'id.name': 'T',
                    'typeParameters': null,
                  },
              },
            ],
            'returnType.type': 'NumberTypeAnnotation',
            'rest': null,
            'typeParameters.params': [
              {
                'type': 'Identifier',
                'name': 'T',
              },
            ]
          },
        }
      ]
    },
    'var a: { add(...rest:Array<number>): number; }': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation.properties': [
        {
          'key.name': 'add',
          'value': {
            'type': 'FunctionTypeAnnotation',
            'params': [],
            'returnType.type': 'NumberTypeAnnotation',
            'rest': {
              'type': 'FunctionTypeParam',
              'name.name': 'rest',
              'typeAnnotation': {
                'type': 'GenericTypeAnnotation',
                'id.name': 'Array',
                'typeParameters.params': [
                  {
                    'type': 'NumberTypeAnnotation',
                  }
                ],
              }
            }
          },
        }
      ]
    },
    'var a:(...rest:Array<number>) => number': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'FunctionTypeAnnotation',
        'params': [],
        'returnType.type': 'NumberTypeAnnotation',
        'rest': {
          'type': 'FunctionTypeParam',
          'name.name': 'rest',
          'typeAnnotation': {
            'type': 'GenericTypeAnnotation',
            'id.name': 'Array',
            'typeParameters.params': [
              {
                'type': 'NumberTypeAnnotation',
              }
            ],
          }
        }
      }
    },
    'var bar: (str:number, i:number)=> string = foo;': {
    },
    'var a:Array<number> = [1, 2, 3]': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'GenericTypeAnnotation',
        'id.name': 'Array',
        'typeParameters.params': [
          {
            'type': 'NumberTypeAnnotation',
          }
        ]
      }
    },
    'function foo(requiredParam, optParam?) {}': {
      'body.0.params': [
        {
          'optional': false,
        },
        {
          'optional': true,
        },
      ]
    },
    'function foo(requiredParam, optParam?=123) {}': {
      'body.0.params': [
        {
          'optional': false,
        },
        {
          'optional': true,
        },
      ],
      'body.0.defaults.1.value': 123,
    },
    'class Foo {set fooProp(value:number){}}': {
      'body.0.body.body.0.value.params.0.typeAnnotation.typeAnnotation': {
        'type': 'NumberTypeAnnotation',
      },
    },
    'a = class Foo<T> { }': {
      'body.0.expression.right.typeParameters.params': [
        {
          'type': 'Identifier',
          'name': 'T',
        }
      ]
    },
    'class Foo<T> {}': {
      'body.0.typeParameters.params': [
        {
          'type': 'Identifier',
          'name': 'T',
        }
      ]
    },
    'class Foo<T> { bar<U>():number { return 42; }}': {
      'body.0.body.body.0.value': {
        'typeParameters.params': [
          {
            'type': 'Identifier',
            'name': 'U',
          },
        ],
        'returnType.typeAnnotation.type': 'NumberTypeAnnotation',
      }
    },
    'class Foo { "bar"<T>() { } }': {
      'body.0.body.body.0.value': {
        'typeParameters.params': [
          {
            'type': 'Identifier',
            'name': 'T',
          },
        ],
        'returnType': null,
      }
    },
    'class Foo { prop1:string; prop2:number; }': {
      'body.0.body.body': [
        {
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
        },
        {
          'typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
        }
      ],
    },
    'class Foo { "prop1":string; }': {
      'body.0.body.body': [
        {
          'key.type': 'Literal',
          'key.value': 'prop1',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'computed': false,
        },
      ],
    },
    'class Foo { 123:string; }': {
      'body.0.body.body': [
        {
          'key.type': 'Literal',
          'key.value': 123,
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'computed': false,
        },
      ],
    },
    'class Foo { [prop1]: string; }' : {
      'body.0.body.body': [
        {
          'key.type': 'Identifier',
          'key.name': 'prop1',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'computed': true
        },
      ]
    },
    'class Foo { [1 + 1]: string; }': {
      'body.0.body.body': [
        {
          'key.type': 'BinaryExpression',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'computed': true
        },
      ]
    },
    'class Foo<T> extends Bar<T> {}': {
      'body.0.typeParameters.params': [
        {
          'type': 'Identifier',
          'name': 'T',
        },
      ],
      'body.0.superTypeParameters.params': [
        {
          'type': 'GenericTypeAnnotation',
          'id.name': 'T',
        },
      ],
    },
    'a = class Foo<T> extends Bar<T> {}': {
      'body.0.expression.right.typeParameters.params': [
        {
          'type': 'Identifier',
          'name': 'T',
        },
      ],
      'body.0.expression.right.superTypeParameters.params': [
        {
          'type': 'GenericTypeAnnotation',
          'id.name': 'T',
        },
      ],
    },
    'var {x}: {x: string; } = { x: "hello" };': {
      'body.0.declarations.0.id': {
        'type': 'ObjectPattern',
        'typeAnnotation.typeAnnotation': {
          'type': 'ObjectTypeAnnotation',
          'properties': [
            {
              'key.name': 'x',
              'value.type': 'StringTypeAnnotation',
            },
          ],
        }
      }
    },
    'var [x]: Array<string> = [ "hello" ];': {
      'body.0.declarations.0.id': {
        'type': 'ArrayPattern',
        'typeAnnotation.typeAnnotation': {
          'type': 'GenericTypeAnnotation',
          'id.name': 'Array',
          'typeParameters.params': [
            {
              'type': 'StringTypeAnnotation',
            },
          ],
        }
      }
    },
    'function foo({x}: { x: string; }) {}': {
      'body.0.params': [
        {
          'type': 'ObjectPattern',
          'typeAnnotation.typeAnnotation': {
            'type': 'ObjectTypeAnnotation',
            'properties': [
              {
                'key.name': 'x',
                'value.type': 'StringTypeAnnotation',
              },
            ],
          }
        },
      ],
    },
    'function foo([x]: Array<string>) {}': {
      'body.0.params': [
        {
          'type': 'ArrayPattern',
          'typeAnnotation.typeAnnotation': {
            'type': 'GenericTypeAnnotation',
            'id.name': 'Array',
            'typeParameters.params': [
              {
                'type': 'StringTypeAnnotation',
              },
            ],
          }
        },
      ],
    },
    'var x : number | string = 4;': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'UnionTypeAnnotation',
        'types': [
          {
            'type': 'NumberTypeAnnotation',
          },
          {
            'type': 'StringTypeAnnotation',
          },
        ],
      }
    },
    'interface Array<T> { concat(...items: Array<Array<T> | T>): Array<T>; }': {
      'body.0.body.properties.0.value.rest.typeAnnotation.typeParameters.params': [
        {
          'type': 'UnionTypeAnnotation',
          'types': [
            {
              'type': 'GenericTypeAnnotation',
              'id.name': 'Array',
              'typeParameters.params': [
                {
                  'type': 'GenericTypeAnnotation',
                  'id.name': 'T',
                },
              ],
            },
            {
              'type': 'GenericTypeAnnotation',
              'id.name': 'T',
              'typeParameters': null,
            },
          ],
        },
      ]
    },
    'var x : () => number | () => string = fn;': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'FunctionTypeAnnotation',
        'returnType': {
          'type': 'UnionTypeAnnotation',
          'types': [
            {
              'type': 'NumberTypeAnnotation',
            },
            {
              'type': 'FunctionTypeAnnotation',
              'returnType.type': 'StringTypeAnnotation',
            },
          ],
        }
      }
    },
    'var x: typeof Y = Y;': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'TypeofTypeAnnotation',
        'argument': {
          'type': 'GenericTypeAnnotation',
          'id.name': 'Y',
        },
      }
    },
    'var x: typeof Y | number = Y;': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'UnionTypeAnnotation',
        'types': [
          {
            'type': 'TypeofTypeAnnotation',
            'argument': {
              'type': 'GenericTypeAnnotation',
              'id.name': 'Y',
            },
          },
          {
            'type': 'NumberTypeAnnotation',
          },
        ],
      }
    },
    'var x : number & string = 4;': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'IntersectionTypeAnnotation',
        'types': [
          {
            'type': 'NumberTypeAnnotation',
          },
          {
            'type': 'StringTypeAnnotation',
          },
        ],
      }
    },
    'interface Array<T> { concat(...items: Array<Array<T> & T>): Array<T>; }': {
      'body.0.body.properties.0.value.rest.typeAnnotation.typeParameters.params': [
        {
          'type': 'IntersectionTypeAnnotation',
          'types': [
            {
              'type': 'GenericTypeAnnotation',
              'id.name': 'Array',
              'typeParameters.params': [
                {
                  'type': 'GenericTypeAnnotation',
                  'id.name': 'T',
                },
              ],
            },
            {
              'type': 'GenericTypeAnnotation',
              'id.name': 'T',
              'typeParameters': null,
            },
          ],
        },
      ]
    },
    'var x : () => number & () => string = fn;': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'FunctionTypeAnnotation',
        'returnType': {
          'type': 'IntersectionTypeAnnotation',
          'types': [
            {
              'type': 'NumberTypeAnnotation',
            },
            {
              'type': 'FunctionTypeAnnotation',
              'returnType.type': 'StringTypeAnnotation',
            },
          ],
        }
      }
    },
    'var x: typeof Y & number = Y;': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'IntersectionTypeAnnotation',
        'types': [
          {
            'type': 'TypeofTypeAnnotation',
            'argument': {
              'type': 'GenericTypeAnnotation',
              'id.name': 'Y',
            },
          },
          {
            'type': 'NumberTypeAnnotation',
          },
        ],
      }
    },
    'var identity: <T>(x: T) => T': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'FunctionTypeAnnotation',
        'typeParameters.params': [
          {
            'name': 'T',
          },
        ],
      },
    }
  },
  'Tuples': {
    'var a : [] = [];': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'TupleTypeAnnotation',
        'types': [],
      },
    },
    'var a : [Foo<T>] = [foo];': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'TupleTypeAnnotation',
        'types': [
          {
            'type': 'GenericTypeAnnotation',
            'typeParameters.params': [
              {
                'type': 'GenericTypeAnnotation',
                'id.name': 'T',
              },
            ],
          },
        ],
      },
    },
    'var a : [number,] = [123,];': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'TupleTypeAnnotation',
        'types': [
          {
            'type': 'NumberTypeAnnotation',
          },
        ],
      },
    },
    'var a : [number, string] = [123, "duck"];': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'TupleTypeAnnotation',
        'types': [
          {
            'type': 'NumberTypeAnnotation',
          },
          {
            'type': 'StringTypeAnnotation',
          },
        ],
        'loc.start.line': 1,
        'loc.start.column': 8,
        'loc.end.line': 1,
        'loc.end.column': 24,
      },
    },
  },
  'Type Aliases': {
    'type FBID = number;': {
      'body': [
        {
          'loc.start.line': 1,
          'loc.start.column': 0,
          'loc.end.line': 1,
          'loc.end.column': 19,
          'type': 'TypeAlias',
          'id.name': 'FBID',
          'typeParameters': null,
          'right': {
            'type': 'NumberTypeAnnotation',
          },
        },
      ],
    },
    'type FBID = number': {
      'body': [
        {
          'loc.start.line': 1,
          'loc.start.column': 0,
          'loc.end.line': 1,
          'loc.end.column': 18,
        },
      ],
    },
    'type Arr<T> = Array<T>;': {
      'body': [
        {
          'type': 'TypeAlias',
          'id.name': 'Arr',
          'typeParameters.params': [
            {
              'type': 'Identifier',
              'name': 'T',
            },
          ],
          'right': {
            'type': 'GenericTypeAnnotation',
            'id.name': 'Array',
            'typeParameters.params': [
              {
                'type': 'GenericTypeAnnotation',
                'id.name': 'T',
                'typeParameters': null
              },
            ],
          },
        },
      ],
    },
  },
  'Interfaces': {
    'interface A {}': {
      'body.0': {
        'id.name': 'A',
        'typeParameters': null,
        'body': {
          'type': 'ObjectTypeAnnotation',
          'properties': [],
        },
        'extends': [],
      }
    },
    'interface A<T, S> {}': {
      'body.0': {
        'id.name': 'A',
        'typeParameters.params': [
          {
            'type': 'Identifier',
            'name': 'T',
          },
          {
            'type': 'Identifier',
            'name': 'S',
          }
        ],
      }
    },
    'interface A { foo: number; }': {
      'body.0.body.properties': [
        {
          'key.name': 'foo',
          'value.type': 'NumberTypeAnnotation',
        }
      ]
    },
    'interface A extends B {}': {
      'body.0.extends': [
        {
          'type': 'InterfaceExtends',
          'id.name': 'B',
          'typeParameters': null,
        },
      ]
    },
    'interface A extends B, C {}': {
      'body.0.extends': [
        {
          'type': 'InterfaceExtends',
          'id.name': 'B',
          'typeParameters': null,
        },
        {
          'type': 'InterfaceExtends',
          'id.name': 'C',
          'typeParameters': null,
        },
      ]
    },
    'interface A<T> extends B<T> {}': {
      'body.0': {
        'id.name': 'A',
        'typeParameters.params': [
          {
            'type': 'Identifier',
            'name': 'T',
          },
        ],
      },
      'body.0.extends': [
        {
          'id.name': 'B',
          'typeParameters.params': [
            {
              'type': 'GenericTypeAnnotation',
              'id.name': 'T',
              'typeParameters': null,
            },
          ],
        }
      ],
    },
    'class Foo implements Bar {}': {
      'body.0.implements': [
        {
          'id.name': 'Bar',
          'typeParameters': null
        }
      ]
    },
    'class Foo extends Bar implements Bat, Man<number> {}': {
      'body.0.superClass.name': 'Bar',
      'body.0.implements': [
        {
          'id.name': 'Bat',
          'typeParameters': null
        },
        {
          'id.name': 'Man',
          'typeParameters.params': [
            {
              'type': 'NumberTypeAnnotation',
            }
          ]
        }
      ]
    },
    'class Foo extends class Bar implements Bat {} {}': {
      'body.0.superClass.implements': [
        {
          'id.name': 'Bat',
          'typeParameters': null
        },
      ],
      'body.0.implements': []
    },
    'class Foo extends class Bar implements Bat {} implements Man {}': {
      'body.0.superClass.implements': [
        {
          'id.name': 'Bat',
          'typeParameters': null
        },
      ],
      'body.0.implements': [
        {
          'id.name': 'Man',
          'typeParameters': null
        },
      ]
    },
    'interface A {numVal: number; [index: number]: string};': {
      'body.0.body': {
        'type': 'ObjectTypeAnnotation',
        'properties': [
          {
            'key.name': 'numVal',
            'value.type': 'NumberTypeAnnotation',
          },
        ],
        'indexers': [{
          'type': 'ObjectTypeIndexer',
          'id.name': 'index',
          'key.type': 'NumberTypeAnnotation',
          'value.type': 'StringTypeAnnotation',
        }]
      }
    },
    'interface A {[index: number]: string; [index2: string]: number};': {
      'body.0.body': {
        'type': 'ObjectTypeAnnotation',
        'properties': [],
        'indexers': [
          {
            'type': 'ObjectTypeIndexer',
            'id.name': 'index',
            'key.type': 'NumberTypeAnnotation',
            'value.type': 'StringTypeAnnotation',
          },
          {
            'type': 'ObjectTypeIndexer',
            'id.name': 'index2',
            'key.type': 'StringTypeAnnotation',
            'value.type': 'NumberTypeAnnotation',
          },
        ]
      }
    },
  },
  'ES6: Computed Properties': {
    '({[x]})': {
      'body.0.expression': {
        'type': 'ObjectExpression',
        'properties': [
          {
            'kind': 'init',
            'computed': true,
            'key.name': 'x',
            'value.name': 'x',
          }
        ],
      }
    }
  },
  'Declare Statements': {
    'declare var foo': {
      'body.0': {
        'type': 'VariableDeclaration',
        'kind': 'var',
        'loc.start.column': 0,
        'loc.end.column': 15,
        'declarations': [
          {
            'type': 'VariableDeclarator',
            'id.name': 'foo',
            'loc.start.column': 12,
            'loc.end.column': 15,
          },
        ],
      },
    },
    'declare var foo;': {
      'body.0': {
        'type': 'VariableDeclaration',
        'kind': 'var',
        'loc.start.column': 0,
        'loc.end.column': 16,
        'declarations': [
          {
            'type': 'VariableDeclarator',
            'id.name': 'foo',
            'loc.start.column': 12,
            'loc.end.column': 15,
          },
        ],
      },
    },
    'declare function foo(): void': {
      'body.0': {
        'type': 'VariableDeclaration',
        'kind': 'var',
        'loc.start.column': 0,
        'loc.end.column': 28,
        'declarations': [
          {
            'type': 'VariableDeclarator',
            'id': {
              'name': 'foo',
              'typeAnnotation.typeAnnotation': {
                'type': 'FunctionTypeAnnotation',
                'params': [],
                'returnType.type': 'VoidTypeAnnotation',
              },
            },
            'loc.start.column': 17,
            'loc.end.column': 28,
          },
        ],
      },
    },
    'declare function foo(): void;': {
      'body.0': {
        'type': 'VariableDeclaration',
        'kind': 'var',
        'loc.start.column': 0,
        'loc.end.column': 29,
        'declarations': [
          {
            'type': 'VariableDeclarator',
            'id': {
              'name': 'foo',
              'typeAnnotation.typeAnnotation': {
                'type': 'FunctionTypeAnnotation',
                'params': [],
                'returnType.type': 'VoidTypeAnnotation',
              },
            },
            'loc.start.column': 17,
            'loc.end.column': 28,
          },
        ],
      },
    },
    'declare function foo(x: number, y: string): void;': {
      'body.0.declarations': [
        {
          'type': 'VariableDeclarator',
          'id': {
            'name': 'foo',
            'typeAnnotation.typeAnnotation': {
              'type': 'FunctionTypeAnnotation',
              'params': [
                {
                  'name.name': 'x',
                  'typeAnnotation.type': 'NumberTypeAnnotation',
                },
                {
                  'name.name': 'y',
                  'typeAnnotation.type': 'StringTypeAnnotation',
                },
              ],
              'returnType.type': 'VoidTypeAnnotation',
            },
          },
        },
      ],
    },
    'declare class A {}': {
      'body.0': {
        'type': 'InterfaceDeclaration',
        'id.name': 'A',
        'typeParameters': null,
        'body.type': 'ObjectTypeAnnotation',
        'body.properties': [],
        'extends': [],
        'loc.start.column': 0,
        'loc.end.column': 18,
      },
    },
    'declare class A<T> extends B<T> { x: number }': {
      'body.0': {
        'type': 'InterfaceDeclaration',
        'id.name': 'A',
        'typeParameters.params': [
          {
            'name': 'T',
          },
        ],
        'body.properties': [
          {
            'key.name': 'x',
            'value.type': 'NumberTypeAnnotation',
            'static': false,
          },
        ],
        'extends': [
          {
            'id.name': 'B',
            'typeParameters.params.0.id.name': 'T',
          },
        ],
      },
    },
    'declare class A { static foo(): number; static x : string; static : number }': {
      'body.0.body.properties.0.static': true,
      'body.0.body.properties.1.static': true,
      'body.0.body.properties.2.static': false
    },
  },
  'Invalid Declaration Statements': {
    // You must provide a return type
    'declare function foo();': {
      'errors': {
        '0': {
          'message': 'Unexpected token ;',
          'loc.start.column': 22,
        },
      }
    },
    // You must provide types for each function parameter
    'declare function foo(x): void': {
      'errors': {
        '0': {
          'message': 'Unexpected token )',
          'loc.start.column': 22,
        },
      },
    },
  },
  'Invalid syntax': {
    // Duplicates are forbidden if IsSimpleParameterList is false, and rest
    // params, patterns, and defaults all make the params non-simple
    'function a(t, t, ...rest) {}': {
      'errors': [
        {
          'message': 'Strict mode function may not have duplicate parameter names',
          'loc': {
            'start': {
              'line': 1,
              'column': 14,
            },
            'end': {
              'line': 1,
              'column': 15,
            }
          },
        }
      ]
    },
    'function a(t, t, [b]) {}': {
      'errors': [
        {
          'message': 'Strict mode function may not have duplicate parameter names',
        }
      ]
    },
    'function a(t, t, {b}) {}': {
      'errors': [
        {
          'message': 'Strict mode function may not have duplicate parameter names',
        }
      ]
    },
    'function a(t, t, b=1) {}': {
      'errors': [
        {
          'message': 'Strict mode function may not have duplicate parameter names',
        }
      ]
    },
    // 11.9.1
    "x\n=>42": {
      'errors': [
        {
          'message': 'Illegal newline before arrow',
          'loc': {
            'start': {
              'line': 2,
              'column': 0,
            },
            'end': {
              'line': 2,
              'column': 2,
            }
          },
        }
      ]
    },
    'try {} catch (-x) {} ': {
      'errors': {
        '0': {
          'message': 'Unexpected token -',
          'loc': {
            'start.column': 14,
            'end.column': 15,
          },
        }
      }
    },
    'try {} catch (answer()) {} ': {
      'errors': {
        '0': {
          'message': 'Unexpected token (',
          'loc': {
            'start.column': 20,
            'end.column': 21,
          },
        }
      }
    },
    'try {} catch (42) {} ': {
      'errors': {
        '0': {
          'message': 'Unexpected number',
          'loc': {
            'start.column': 14,
            'end.column': 16,
          },
        }
      }
    },
    // 11.8.5.1 no unicode as flags
    'var x = /[P QR]/\\\\u0067': {
      'errors': {
        '0': {
          'message': 'Unexpected token ILLEGAL',
          'loc': {
            'start.column': 16,
            'end.column': 17,
          },
        }
      }
    },
  },
  'Invalid XJS Syntax': {
    '(<div />) < x;': {
      'errors': {
        '0': {
          'message': 'Unexpected token <. Remember, adjacent XJS elements '+
              'must be wrapped in an enclosing parent tag',
          'loc': {
            'start.column': 10,
            'end.column': 11,
          },
        }
      }
    },
    'var x = <div>one</div><div>two</div>;': {
      'errors': {
        '0': {
          'message': 'Unexpected token <. Remember, adjacent XJS elements '+
              'must be wrapped in an enclosing parent tag',
          'loc': {
            'start.column': 22,
            'end.column': 23,
          },
        }
      }
    },
    'var x = <div>one</div> /* intervening comment */ <div>two</div>;': {
      'errors': {
        '0': {
          'message': 'Unexpected token <. Remember, adjacent XJS elements '+
              'must be wrapped in an enclosing parent tag',
          'loc': {
            'start.column': 49,
            'end.column': 50,
          },
        }
      }
    }
  },
  'Type Grouping': {
    'var a: (number)': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'NumberTypeAnnotation',
        'loc.start.column': 8,
        'loc.end.column': 14,
      },
    },
    'var a: (() => number) | () => string': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation.types': [
        {
          'type': 'FunctionTypeAnnotation',
          'returnType.type': 'NumberTypeAnnotation',
        },
        {
          'type': 'FunctionTypeAnnotation',
          'returnType.type': 'StringTypeAnnotation',
        },
      ],
    },
    'var a: number & (string | bool)': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'IntersectionTypeAnnotation',
        'types.0.type': 'NumberTypeAnnotation',
        'types.1.type': 'UnionTypeAnnotation',
        'types.1.types.0.type': 'StringTypeAnnotation',
        'types.1.types.1.type': 'BooleanTypeAnnotation',
      },
    },
    'var a: (typeof A)': {},
    'var a: Array<(number)>': {},
    'var a: ([]) = []': {},
  },
  'String Literal Types': {
    'var a: "duck"': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'StringLiteralTypeAnnotation',
        'value': 'duck',
        'raw': '"duck"',
      }
    },
    'var a: \'duck\'': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'StringLiteralTypeAnnotation',
        'value': 'duck',
        'raw': '\'duck\'',
      }
    },
  },
  'Invalid String Literal Types': {
    'var a: "\\01"': {
      'errors': [
        {
          'message': 'Octal literals are not allowed in strict mode.',
        }
      ],
    }
  },
  'Member Type': {
    'var a : A.B': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'GenericTypeAnnotation',
        'id': {
          'type': 'QualifiedTypeIdentifier',
          'qualification.name': 'A',
          'id.name': 'B'
        }
      }
    },
    'var a : A.B.C': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'GenericTypeAnnotation',
        'id': {
          'type': 'QualifiedTypeIdentifier',
          'qualification': {
            'type': 'QualifiedTypeIdentifier',
            'qualification.name': 'A',
            'id.name': 'B',
          },
          'id.name': 'C'
        }
      }
    },
    'var a : A.B<T>': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'GenericTypeAnnotation',
        'id': {
          'type': 'QualifiedTypeIdentifier',
          'qualification.name': 'A',
          'id.name': 'B'
        },
        'typeParameters.params': [
          {
            'type': 'GenericTypeAnnotation',
            'id.name': 'T',
          }
        ]
      }
    },
    'var a : typeof A.B<T>': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
        'type': 'TypeofTypeAnnotation',
        'argument': {
          'type': 'GenericTypeAnnotation',
          'id': {
            'type': 'QualifiedTypeIdentifier',
            'qualification.name': 'A',
            'id.name': 'B'
          },
          'typeParameters.params': [
            {
              'type': 'GenericTypeAnnotation',
              'id.name': 'T',
            }
          ]
        }
      }
    }
  },
  'Declare Module': {
    'declare module A {}': {
      'body.0': {
        'type': 'DeclareModule',
        'id.name': 'A',
        'body.body': [],
      }
    },
    'declare module A { declare module B {} }': {
      'body.0.body.body': [
        {
          'type': 'DeclareModule',
          'id.name': 'B',
          'body.body': []
        },
      ]
    },
    'declare module A { export default function foo() {} }': {
      'body.0.body.body.0.type': 'ExportDeclaration',
    }
  },
  'Invalid Declare Module': {
    'declare Module A {}': {
      'errors': {
        '0.message': 'Unexpected identifier',
      }
    },
    'declare module {}': {
      'errors': {
        '0.message': 'Unexpected token {',
      }
    },
  },
  'Call Properties': {
    'var a : { (): number }': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation.callProperties': [
        {
          'type': 'FunctionTypeAnnotation',
          'params': [],
          'returnType.type': 'NumberTypeAnnotation',
          'typeParameters': null,
        }
      ],
    },
    'var a : { (): number; }': {},
    'var a : { (): number; y: string; (x: string): string }': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation.callProperties': [
        {
          'type': 'FunctionTypeAnnotation',
          'params': [],
          'returnType.type': 'NumberTypeAnnotation',
        },
        {
          'type': 'FunctionTypeAnnotation',
          'params': [{
            'type': 'FunctionTypeParam',
            'name.name': 'x',
            'typeAnnotation.type': 'StringTypeAnnotation',
          }],
          'returnType.type': 'StringTypeAnnotation',
        },
      ],
    },
    'var a : { <T>(x: T): number; }': {
      'body.0.declarations.0.id.typeAnnotation.typeAnnotation.callProperties': [
        {
          'typeParameters.params': [
            {
              'name': 'T',
            }
          ],
        }
      ],
    },
    'interface A { (): number; }': {
      'body.0.body.callProperties': [
        {
          'type': 'FunctionTypeAnnotation',
          'params': [],
          'returnType.type': 'NumberTypeAnnotation',
          'typeParameters': null,
        }
      ]
    }
  },
  'Invalid Call Properties': {
    'var a : { () }': {
      'errors': {
        '0.message': 'Unexpected token }',
      }
    },
  }
};
