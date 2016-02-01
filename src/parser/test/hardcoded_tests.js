module.exports = {
  'todo': {
  },
  'sections': {
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
      'function foo(a: function, b: switch){}': {
        'body.0.params.0.typeAnnotation.typeAnnotation.id.name': 'function',
        'body.0.params.1.typeAnnotation.typeAnnotation.id.name': 'switch',
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
      'class Foo { static prop: number; }': {
        'body.0.body.body': [
          {
            'key.name': 'prop',
            'static': true,
          }
        ]
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
            'type': 'ClassProperty',
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
      // type param variances are not carried in the translated AST yet
      'class Foo<+T1,-T2> {}': {
        'body.0.typeParameters.params': [
          {
            'type': 'Identifier',
            'name': 'T1',
          },
          {
            'type': 'Identifier',
            'name': 'T2',
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
      },
      'var a: (number: any, void: any, string: any, any: any, type: any, static: any) => any;': {},
      'var a: {[type: any]: any}': {},
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
    'Typecasts': {
      '(xxx: number)': {
        'body.0.expression': {
          'type': 'TypeCastExpression',
          'expression.type': 'Identifier',
          'typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation'
        }
      },
      '({xxx: 0, yyy: "hey"}: {xxx: number; yyy: string})': {
        'body.0.expression': {
          'type': 'TypeCastExpression',
          'expression.type': 'ObjectExpression',
          'typeAnnotation.typeAnnotation.type': 'ObjectTypeAnnotation'
        }
      },
      // distinguish between function type params and typecasts
      '((xxx) => xxx + 1: (xxx: number) => number)': {
        'body.0.expression': {
          'type': 'TypeCastExpression',
          'expression.type': 'ArrowFunctionExpression',
          'typeAnnotation.typeAnnotation': {
            'type': 'FunctionTypeAnnotation',
            'params.0.name.name': 'xxx'
          }
        }
      },
      // parens disambiguate groups from casts
      '((xxx: number), (yyy: string))': {
        'body.0.expression': {
          'type': 'SequenceExpression',
          'expressions.0.type': 'TypeCastExpression',
          'expressions.1.type': 'TypeCastExpression',
        }
      },
    },
    'Invalid Typecasts': {
      // Must be parenthesized
      'var x: number = 0: number;': {
        'errors': {
          '0': {
            'message': 'Unexpected token :',
            'loc.start.column': 17,
          }
        }
      },
      // ...even within groups
      '(xxx: number, yyy: string)': {
        'errors': {
          '0': {
            'message': 'Unexpected token ,',
            'loc.start.column': 12,
          }
        }
      }
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
    'ES6: Modules': {
      'export class {}': {
        'body.0.declaration': {
          'type': 'ClassExpression',
        }
      },
      'export default class {}': {
        'body.0.declaration': {
          'type': 'ClassExpression',
        }
      },
      'export default class A {}': {
        'body.0.declaration': {
          'type': 'ClassDeclaration',
        }
      },
      'export type A = number': {
        'body.0.declaration': {
          'type': 'TypeAlias',
        }
      },
      'export interface foo {p: number}': {
        'body.0.declaration': {
          'type': 'InterfaceDeclaration',
        }
      },
      'export * as foo from "bar"': {
        '%parse_options%': {
          "esproposal_export_star_as": true
        },
        'body.0.specifiers': [{
          'type': 'ExportBatchSpecifier',
          'name': {
            'type': 'Identifier',
            'name': 'foo'
          },
        }]
      }
    },
    'Declare Statements': {
      'declare var foo': {
        'body': [{
          'type': 'DeclareVariable',
          'loc.start.column': 0,
          'loc.end.column': 15,
          'id.name': 'foo',
          'id.typeAnnotation': null,
        }],
      },
      'declare var foo;': {
        'body': [{
          'type': 'DeclareVariable',
          'loc.start.column': 0,
          'loc.end.column': 16,
          'id.name': 'foo',
          'id.typeAnnotation': null,
        }],
      },
      'declare function foo(): void': {
        'body': [{
          'type': 'DeclareFunction',
          'loc.start.column': 0,
          'loc.end.column': 28,
          'id': {
            'name': 'foo',
            'typeAnnotation.typeAnnotation': {
              'type': 'FunctionTypeAnnotation',
              'params': [],
              'returnType.type': 'VoidTypeAnnotation',
            },
          },
        }],
      },
      'declare function foo(): void;': {
        'body': [{
          'type': 'DeclareFunction',
          'loc.start.column': 0,
          'loc.end.column': 29,
          'id': {
            'name': 'foo',
            'typeAnnotation.typeAnnotation': {
              'type': 'FunctionTypeAnnotation',
              'params': [],
              'returnType.type': 'VoidTypeAnnotation',
            },
          },
        }],
      },
      'declare function foo(x: number, y: string): void;': {
        'body.0': {
          'type': 'DeclareFunction',
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
      },
      'declare class A {}': {
        'body.0': {
          'type': 'DeclareClass',
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
          'type': 'DeclareClass',
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
      'declare class A { static foo(): number; static x : string; }': {
        'body.0.body.properties.0.static': true,
        'body.0.body.properties.1.static': true,
      },
      'declare class A { static () : number }': {
        'body.0.body': {
          'properties': [],
          'callProperties': [
            {
              'static': true,
            }
          ],
          'indexers': [],
        }
      },
      'declare class A { static [ indexer: number]: string }': {
        'body.0.body': {
          'properties': [],
          'callProperties': [],
          'indexers': [
            {
              'static': true,
            }
          ]
        }
      },
    },
    'Declare Export': {
      // Batch export
      'declare export * from "foo";': {
        'body': [
          {
            'type': 'DeclareExportDeclaration',
            'specifiers': [
              {
                'type': 'ExportBatchSpecifier',
              }
            ],
            'source.value': "foo",
            'declaration': null,
            'default': false,
          },
        ],
      },
      'declare export * from "foo"': {
        'body': [
          {
            'type': 'DeclareExportDeclaration',
            'specifiers': [
              {
                'type': 'ExportBatchSpecifier',
              }
            ],
            'source.value': "foo",
            'declaration': null,
            'default': false,
          },
        ],
      },

      // Named value exports
      'declare export {} from "foo";': {
        'body.0.specifiers': [],
        'body.0.default': false,
        'body.0.source.value': "foo",
      },
      'declare export { bar } from "foo";': {
        'body.0.specifiers': [
          {
            'type': 'ExportSpecifier',
            'id.name': 'bar',
          },
        ],
      },
      'declare export { bar } from "foo"': {},
      'declare export { bar, baz } from "foo";': {
        'body.0.specifiers': [
          {
            'id.name': 'bar',
          },
          {
            'id.name': 'baz',
          },
        ],
      },
      'declare export { bar };': {},
      'declare export { bar, }': {},
      'declare export { bar, baz };': {},

      // Variable export
      'declare export var x': {
        'body.0.declaration': {
          'type': 'DeclareVariable',
          'id.name': 'x',
          'id.typeAnnotation': null,
        },
      },
      'declare export var x;': {},
      'declare export var x: number;': {
        'body.0.declaration': {
          'type': 'DeclareVariable',
          'id.name': 'x',
          'id.typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
        },
      },

      // Function export
      'declare export function foo(): void': {
        'body.0.declaration': {
          'type': 'DeclareFunction',
          'id.name': 'foo',
          'id.typeAnnotation.typeAnnotation': {
            'type': 'FunctionTypeAnnotation',
            'params': [],
            'returnType.type': 'VoidTypeAnnotation',
          },
        },
      },
      'declare export function foo(): void;': {},
      'declare export function foo<T>(): void;': {},
      'declare export function foo(x: number, y: string): void;': {},

      // Class export
      'declare export class A {}': {
        'body.0.declaration': {
          'type': 'DeclareClass',
          'id.name': 'A',
          'typeParameters': null,
          'body': {
            'type': 'ObjectTypeAnnotation',
            'properties': []
          },
        },
      },
      'declare export class A<T> extends B<T> { x: number }': {
        'body.0.declaration': {
          'type': 'DeclareClass',
          'id.name': 'A',
          'typeParameters.params': [
            {
              'name': 'T',
            },
          ],
          'body': {
            'type': 'ObjectTypeAnnotation',
            'properties': [
              {
                'key.name': 'x',
              },
            ]
          },
        },
      },
      'declare export class A { static foo(): number; static x : string }': {},
      'declare export class A { static [ indexer: number]: string }': {},
      'declare export class A { static () : number }': {},

      // Default export. This corresponds to something like export default 1+1
      'declare export default number;': {
        'body.0.declaration.type': 'NumberTypeAnnotation',
        'body.0.default': true,
      },
      'declare export default number': {
      },

      // Default function export
      'declare export default function foo(): void': {
        'body.0.declaration': {
          'type': 'DeclareFunction',
          'id.name': 'foo',
          'id.typeAnnotation.typeAnnotation': {
            'type': 'FunctionTypeAnnotation',
            'params': [],
            'returnType.type': 'VoidTypeAnnotation',
          },
        },
        'body.0.default': true,
      },
      'declare export default function foo(): void;': {
      },
      'declare export default function foo<T>(): void;': {
      },
      'declare export default function foo(x: number, y: string): void;': {
      },

      // Default class export
      'declare export default class A {}': {
        'body.0.declaration': {
          'type': 'DeclareClass',
          'id.name': 'A',
          'typeParameters': null,
          'body': {
            'type': 'ObjectTypeAnnotation',
            'properties': []
          },
        },
        'body.0.default': true,
      },
      'declare export default class A<T> extends B<T> { x: number }': {
      },
      'declare export default class A { static foo(): number; static x : string }': {
      },
      'declare export default class A { static [ indexer: number]: string }': {
      },
      'declare export default class A { static () : number }': {
      },
    },
    'Invalid Declare Export': {
      // declare export type is not supported since export type is identical
      'declare export type foo = number;': {
        'errors.0.message': '`declare export type` is not supported. Use `export type` instead.',
      },
      'declare export type { x, y }': {
        'errors.0.message': '`declare export type` is not supported. Use `export type` instead.',
      },
      'declare export type * from "foo";': {
        'errors.0.message': '`declare export type` is not supported. Use `export type` instead.',
      },
      'declare export type var x: number;': {
        'errors.0.message': '`declare export type` is not supported. Use `export type` instead.',
      },
      'declare export type function foo(): void': {
        'errors.0.message': '`declare export type` is not supported. Use `export type` instead.',
      },
      'declare export type class A {}': {
        'errors.0.message': '`declare export type` is not supported. Use `export type` instead.',
      },
      'declare export type default number;': {
        'errors.0.message': '`declare export type` is not supported. Use `export type` instead.',
      },
      'declare export type default function foo(): void': {
        'errors.0.message': '`declare export type` is not supported. Use `export type` instead.',
      },
      'declare export type default class A {}': {
        'errors.0.message': '`declare export type` is not supported. Use `export type` instead.',
      },
      // declare export let and declare export const are not supported
      'declare export let foo: number': {
        'errors.0.message': '`declare export let` is not supported. Use `declare export var` instead.',
      },
      'declare export const foo: number': {
        'errors.0.message': '`declare export const` is not supported. Use `declare export var` instead.',
      },
      // You may only export a type directly for declare export default
      'declare export number;': {
        'errors.0.message': 'Unexpected identifier',
        'errors.0.loc.start.column': 15,
      },
      // You must provide a return type
      'declare export function foo();': {
        'errors': {
          '0': {
            'message': 'Unexpected token ;',
            'loc.start.column': 29,
          },
        }
      },
      // You must provide types for each function parameter
      'declare export function foo(x): void': {
        'errors': {
          '0': {
            'message': 'Unexpected token )',
            'loc.start.column': 29,
          },
        },
      },
      'declare export class A { static : number }': {
        'body.0.declaration.body.properties.0.static': false,
        'errors': [
          {
            'message': 'Use of future reserved word in strict mode',
          }
        ]
      },
      'declare export class A { static implements: number; implements: number }': {
        'body.0.declaration': {
          'type': 'DeclareClass',
          'id.name': 'A',
          'body.properties': [
            {
              'key.name': 'implements',
              'value.type': 'NumberTypeAnnotation',
              'static': true,
            },
            {
              'key.name': 'implements',
              'value.type': 'NumberTypeAnnotation',
              'static': false,
            },
          ],
        },
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
      'declare class A { static : number }': {
        'body.0.body.properties.0.static': false,
        'errors': [
          {
            'message': 'Use of future reserved word in strict mode',
          }
        ]
      },
      'declare class A { static implements: number; implements: number }': {
        'body.0': {
          'type': 'DeclareClass',
          'id.name': 'A',
          'body.properties': [
            {
              'key.name': 'implements',
              'value.type': 'NumberTypeAnnotation',
              'static': true,
            },
            {
              'key.name': 'implements',
              'value.type': 'NumberTypeAnnotation',
              'static': false,
            },
          ],
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
      "[x, y] => 123": {
        'errors': {
          '0': {
            'message': 'Unexpected token =>',
          }
        },
      },
      "({a: x, b: y} => 123)": {
        'errors': {
          '0': {
            'message': 'Unexpected token =>',
          }
        },
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
      // No generics for getters and setters
      '({ get foo<T>() {} })': {
        'errors': {
          '0.message': 'Unexpected token <',
        },
      },
      '({ set foo<T>(newFoo) {} })': {
        'errors': {
          '0.message': 'Unexpected token <',
        },
      },
      '1 + enum': {
        'errors': {
          '0.message': 'Unexpected reserved word',
        }
      },
      'enum = 42': {
        'errors': {
          '0.message': 'Unexpected reserved word',
        }
      },
      'var enum': {
        'errors': {
          '0.message': 'Unexpected reserved word',
        }
      },
      'function hello() { "use strict"; var enum; }': {
        'errors': {
          '0.message': 'Unexpected reserved word',
        }
      },
    },
    'Invalid JSX Syntax': {
      '(<div />) < x;': {
        'errors': {
          '0': {
            'message': 'Unexpected token <. Remember, adjacent JSX elements '+
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
            'message': 'Unexpected token <. Remember, adjacent JSX elements '+
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
            'message': 'Unexpected token <. Remember, adjacent JSX elements '+
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
      'var a: "foo bar"': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'StringLiteralTypeAnnotation',
          'value': 'foo bar',
          'raw': '"foo bar"',
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
    'Number Literal Types': {
      'var a: 123': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'NumberLiteralTypeAnnotation',
          'value': 123,
          'raw': '123',
        }
      },
      'var a: 123.0': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'NumberLiteralTypeAnnotation',
          'value': 123,
          'raw': '123.0',
        }
      },
      'var a: 0x7B': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'NumberLiteralTypeAnnotation',
          'value': 123,
          'raw': '0x7B',
        }
      },
      'var a: 0b1111011': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'NumberLiteralTypeAnnotation',
          'value': 123,
          'raw': '0b1111011',
        }
      },
      'var a: 0o173': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'NumberLiteralTypeAnnotation',
          'value': 123,
          'raw': '0o173',
        }
      },
      'var a: -123': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'NumberLiteralTypeAnnotation',
          'value': -123,
          'raw': '-123',
        }
      },
      'var a: - 123': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'NumberLiteralTypeAnnotation',
          'value': -123,
          'raw': '- 123',
        }
      },
    },
    'Invalid Number Literal Types': {
      'var a: 0173': {
        'errors': [
          {
            'message': 'Octal literals are not allowed in strict mode.',
          }
        ],
      },
      'var a: 123-3': {
        'errors': [
          {
            'message': 'Unexpected token -',
          }
        ],
      },
    },
    'Boolean Literal Types': {
      'var a: true': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'BooleanLiteralTypeAnnotation',
          'value': true,
          'raw': 'true',
        }
      },
      'var a: false': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'BooleanLiteralTypeAnnotation',
          'value': false,
          'raw': 'false',
        }
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
    'Arrow Function': {
      '(x => 123)': {
        'body.0.expression.params': [
          {
            'type': 'Identifier',
          }
        ]
      },
      '((x) => 123)': {
        'body.0.expression.params': [
          {
            'type': 'Identifier',
          }
        ]
      },
      '(([x]) => 123)': {
        'body.0.expression.params': [
          {
            'type': 'ArrayPattern',
          }
        ]
      },
      '(({x}) => 123)': {
        'body.0.expression.params': [
          {
            'type': 'ObjectPattern',
          }
        ]
      },
      '(({x: y}) => 123)': {
        'body.0.expression.params': [
          {
            'type': 'ObjectPattern',
          }
        ]
      },
      '(x: number): number => x': {
        'body.0.expression': {
          'params': [
            {
              'name': 'x',
              'typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
            },
          ],
          'returnType.typeAnnotation.type': 'NumberTypeAnnotation',
        },
      },
      '<T>(x: T): T => x': {
        'body.0.expression.typeParameters.params': [
          {
            'name': 'T',
          }
        ],
      },
    },
    'Invalid Arrow Functions': {
      'var f = x: number => 42': {
        'errors': {
          '0.message': 'Unexpected token :',
        }
      },
      'label: typeThatIsActuallyAnParam => 42': {
        'body': [
          {
            'type': 'LabeledStatement',
            'body.expression.type': 'ArrowFunctionExpression',
          },
        ],
      },
      '<T>x => 42': {
        'errors': {
          '0.message': 'Unexpected token ILLEGAL',
        },
      },
      '*x => x': {
        'errors': {
          '0.message': 'Unexpected token *',
        },
      },
      '*(x) => x': {
        'errors': {
          '0.message': 'Unexpected token *',
        },
      },
    },
    'Declare Module': {
      'declare module A {}': {
        'body.0': {
          'type': 'DeclareModule',
          'id.name': 'A',
          'body.body': [],
        }
      },
      'declare module "./a/b.js" {}': {
        'body.0.id': {
          'type': 'Literal',
          'value': './a/b.js',
        }
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
      'declare module A { declare module B {} }': {
        'errors': {
          '0.message': 'Unexpected identifier',
        }
      },
      'declare module A { export default function foo() {} }': {
        'errors': {
          '0.message': 'Unexpected token export',
        }
      },
    },
    'Call Properties': {
      'var a : { (): number }': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation.callProperties': [
          {
            'type': 'ObjectTypeCallProperty',
            'value': {
              'type': 'FunctionTypeAnnotation',
              'params': [],
              'returnType.type': 'NumberTypeAnnotation',
              'typeParameters': null,
            },
            'static': false,
          }
        ],
      },
      'var a : { (): number; }': {},
      'var a : { (): number; y: string; (x: string): string }': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation.callProperties': [
          {
            'type': 'ObjectTypeCallProperty',
            'value': {
              'type': 'FunctionTypeAnnotation',
              'params': [],
              'returnType.type': 'NumberTypeAnnotation',
            },
            'static': false,
          },
          {
            'type': 'ObjectTypeCallProperty',
            'value': {
              'type': 'FunctionTypeAnnotation',
              'params': [{
                'type': 'FunctionTypeParam',
                'name.name': 'x',
                'typeAnnotation.type': 'StringTypeAnnotation',
              }],
              'returnType.type': 'StringTypeAnnotation',
            },
            'static': false,
          },
        ],
      },
      'var a : { <T>(x: T): number; }': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation.callProperties': [
          {
            'value.typeParameters.params': [
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
            'type': 'ObjectTypeCallProperty',
            'value': {
              'type': 'FunctionTypeAnnotation',
              'params': [],
              'returnType.type': 'NumberTypeAnnotation',
              'typeParameters': null,
            },
            'static': false,
          },
        ]
      }
    },
    'Invalid Call Properties': {
      'var a : { () }': {
        'errors': {
          '0.message': 'Unexpected token }',
        }
      },
    },
    'Invalid type keywords': {
      'type string = number;': {
        'errors': {
          '0.message': 'Unexpected token string',
        }
      },
      'type foo<number> = number': {
        'errors': {
          '0.message': 'Unexpected token number',
        }
      },
    },
    'Invalid Import Types': {
      'import type "MyModule"': {
        'errors': {
          '0.message': 'Unexpected string',
        }
      }
    },
    'Type Annotations In Comments': {
      'function foo(numVal/*: number*/, x/* : number*/){}': {
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
      'function foo(a/* :function*/, b/*  : switch*/){}': {
        'body.0.params.0.typeAnnotation.typeAnnotation.id.name': 'function',
        'body.0.params.1.typeAnnotation.typeAnnotation.id.name': 'switch',
      },
      'function foo(numVal/*::: number*/, strVal/*:: :string*/){}': {
        'body.0': {
          'params': {
            '0.typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
            '1.typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation'
          },
          'returnType': null,
          'typeParameters': null,
        }
      },
      'function foo(numVal/*  :: : number*/, untypedVal){}': {
        'body.0.params.0.typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
        'body.0.params.1.typeAnnotation': null
      },
      'function foo(untypedVal, numVal/*flow-include: number*/){}': {
        'body.0.params.0.typeAnnotation': null,
        'body.0.params.1.typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation'
      },
      'function foo(nullableNum/*flow-include : ?number*/){}': {
        'body.0.params.0.typeAnnotation.typeAnnotation': {
          'type': 'NullableTypeAnnotation',
          'typeAnnotation.type': 'NumberTypeAnnotation'
        }
      },
      'function foo(callback/* flow-include : () => void*/){}': {
        'body.0.params.0.typeAnnotation.typeAnnotation': {
          'type': 'FunctionTypeAnnotation',
          'params': [],
          'returnType.type': 'VoidTypeAnnotation',
        }
      },
      'function foo(callback/*: () => number*/){}': {
        'body.0.params.0.typeAnnotation.typeAnnotation': {
          'type': 'FunctionTypeAnnotation',
          'params': [],
          'returnType.type': 'NumberTypeAnnotation',
        }
      },
      'function foo(callback/*: (_:bool) => number*/){}': {
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
      'function foo(callback/*: (_1:bool, _2:string) => number*/){}': {
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
      'function foo()/*:number*/{}': {
        'body.0.returnType.typeAnnotation.type': 'NumberTypeAnnotation',
      },
      'function foo()/*:() => void*/{}': {
        'body.0.returnType.typeAnnotation': {
          'type': "FunctionTypeAnnotation",
          'params': [],
          'returnType.type': 'VoidTypeAnnotation',
        }
      },
      "/*::\ntype duck = {\n  quack(): string;\n};\n*/": {
        'body': [
          {
            'type': 'TypeAlias',
            'id.name': 'duck',
            'typeParameters': null,
            'right': {
              'type': 'ObjectTypeAnnotation',
            },
          },
        ],
      },
      "/*flow-include\ntype duck = {\n  quack(): string;\n};\n*/": {
        'body': [
          {
            'type': 'TypeAlias',
            'id.name': 'duck',
            'typeParameters': null,
            'right': {
              'type': 'ObjectTypeAnnotation',
            },
          },
        ],
      },
      '/*:: */': {
        'body': [],
      },
      '/*flow-include */': {
        'body': [],
      },
      'function foo/*:: <T> */(x /*: T */)/*: T */ { return x; }': {
        'body.0.typeParameters.params': [
          {
            'type': 'Identifier',
            'name': 'T',
          }
        ]
      },
      '/*::type F = /* inner escaped comment *-/ number;*/': {
        'body': [
          {
            'type': 'TypeAlias',
            'id.name': 'F',
            'typeParameters': null,
            'right': {
              'type': 'NumberTypeAnnotation',
            },
          }
        ],
        'comments': [
          {
            'type': 'Block',
            'value': ' inner escaped comment '
          }
        ]
      },
      '/*flow-include type F = /* inner escaped comment *-/ number;*/': {
        'body': [
          {
            'type': 'TypeAlias',
            'id.name': 'F',
            'typeParameters': null,
            'right': {
              'type': 'NumberTypeAnnotation',
            },
          }
        ],
        'comments': [
          {
            'type': 'Block',
            'value': ' inner escaped comment '
          }
        ]
      },
      'var a/*: /* inner escaped comment *-/ number*/;': {
        'body': [
          {'type': 'VariableDeclaration'}
        ],
        'comments': [
          {
            'type': 'Block',
            'value': ' inner escaped comment '
          }
        ]
      },
    },
    'Invalid Type Annotations In Comments': {
      '/*: */': {
        'errors': {
          '0.message': 'Unexpected token /*:',
        }
      },
      '/*:: /*: */': {
        'errors': {
          '0.message': 'Unexpected token /*:',
        }
      },
      '/*:: /* : */': {
        'errors': {
          '0.message': 'Unexpected token /* :',
        }
      },
      '/*:: /*:: */': {
        'errors': {
          '0.message': 'Unexpected token /*::',
        }
      },
      '/*:: /*flow-include */': {
        'errors': {
          '0.message': 'Unexpected token /*flow-include',
        }
      },
      '*/': {
        'errors': {
          '0.message': 'Unexpected token */',
        }
      },
      '/*::': {
        'errors': {
          '0.message': 'Unexpected end of input',
        }
      },
      '/*:: type PowerGlove = /* bad means good */ soBad; */': {
        'errors': {
          '0.message': 'Unexpected token `*/`. Did you mean `*-/`?'
        }
      },
    },
    'Trailing commas': {
      'Math.max(a, b, c,)': {},
      'var exp = function(a, b,) { return a + b; };': {},
      'function dec(a, b,) { return a + b; }': {},
      'class Test { constructor(x, y,) {} }': {},
      '(x,) => x * x': {},
      '(x,y,) => Math.pow(x,y,)': {},
      '(function foo(x = 5,) {})': {},
      'foo(a, ...b,)': {},
      'var x = [1, 2, 3,];': {},
      'var x = [1, 2, ...y,];': {},
      'var x = [1, 2, ...y, 4,];': {},
    },
    'Invalid trailing commas': {
      'foo(a, (b,))': {
        'errors': {
          '0.message': 'Unexpected token )',
        }
      },
      'function foo(a, ...b,) { return b.concat(a); }': {
        'errors': {
          '0.message': 'Rest parameter must be final parameter of an argument list',
        }
      },
      'var f = function(a, ...b,) { return b.concat(a); }': {
        'errors': {
          '0.message': 'Rest parameter must be final parameter of an argument list',
        }
      },
      'var f = (a, ...b,) => b.concat(a);': {
        'errors': {
          '0.message': 'Unexpected token ...',
        }
      },
    },
    'Invalid For Of Loops': {
      'for (var x = 42 of list) process(x);': {
        'errors': {
          '0.message': 'Invalid left-hand side in for-of',
        },
      },
    },
    'Async/Await': {
      'async: while (async) { continue async; }': {},
      'await: while (await) { continue await; }': {},
      'var await = { await }': {},
      'var async = { async }': {},
      'var async = { async : foo }': {},
      'async function f() { var await = { await : async function foo() {} } }':
        {},
      'async function f(async, await) { var x = await async; return x; }': {},
      'function f() { return await; }': {},
      'async function f() { return await; }': {
        'errors': {
          // inside an async function, await is considered a keyword
          '0.message': 'Unexpected token ;',
        },
      },
      'function f(x: async) : async { return x; }': {},
      'declare async function foo() : T': {
        'errors': {
          '0.message': "async is an implementation detail and isn't necessary " +
            "for your declare function statement. It is sufficient for your " +
            "declare function to just have a Promise return type.",
        },
      },
      'declare async function async(async : async) : async': {
        'errors': {
          '0.message': "async is an implementation detail and isn't necessary " +
            "for your declare function statement. It is sufficient for your " +
            "declare function to just have a Promise return type.",
        },
      },
      'declare async function await(await : await) : await': {
        'errors': {
          '0.message': "async is an implementation detail and isn't necessary " +
            "for your declare function statement. It is sufficient for your " +
            "declare function to just have a Promise return type.",
        },
      },
      'declare function foo() : Promise<async>': {},
      'declare function foo() : Promise<await>': {},
      'declare function async() : bar': {},
      'async function foo() { var await = 4; }': {},
      'async function foo() { var await = 4; return await; }': {
        'errors': {
          '0.message': 'Unexpected token ;',
        },
      },
      // esprima chokes on these
      'export const async = 5': {},
      'export const await = 5': {},
      'export const foo = async function() { }': {},
      'export const foo = async () => y': {},
      'export function async() { }': {},
      'export function await() { }': {},
      'export async function foo(x) { await x; }': {},
      "import async from 'foo'": {},
      "import await from 'foo'": {},
    },
    'Invalid Async Generators': {
      'async function *foo() {}' : {
        'errors': {
          '0.message': 'A function may not be both async and a generator',
        },
      },
      'async function *ft<T>(a: T): void {}' : {
        'errors': {
          '0.message': 'A function may not be both async and a generator',
        },
      },
      'class C { async *m() {} }' : {
        'errors': {
          '0.message': 'A function may not be both async and a generator',
        },
      },
      'class C { async *mt<T>(a: T): void {} }' : {
        'errors': {
          '0.message': 'A function may not be both async and a generator',
        },
      },
      'class C { static async *m(a): void {} }' : {
        'errors': {
          '0.message': 'A function may not be both async and a generator',
        },
      },
      'class C { static async *mt<T>(a: T): void {} }' : {
        'errors': {
          '0.message': 'A function may not be both async and a generator',
        },
      },
      'var e = async function *() {};' : {
        'errors': {
          '0.message': 'A function may not be both async and a generator',
        },
      },
      'var et = async function*<T> (a: T): void {};' : {
        'errors': {
          '0.message': 'A function may not be both async and a generator',
        },
      },
      'var n = new async function*() {};' : {
        'errors': {
          '0.message': 'A function may not be both async and a generator',
        },
      },
    },
    'Async Arrow Functions': {
      'var x = async () => await promise;': {},
      'var x = async (a) => await a;': {},
      'var x = async a => await a;': {},
      'var x = async => async + 1;': {},
      'var x = async (a => a + 1);': {},
      'var x = async(x)': {},
      'var x = async (a, b) => await a + b;': {
        'body.0.declarations.0.init.body': {
          'type': 'BinaryExpression',
        },
      },
      'var x = async (a, b, c, d, e, f, g) => await a + await b + c + d + e + f + g;': {},
      'var x = 1 y => y': {
        'errors': {
          '0.message': 'Unexpected identifier',
        },
      },
      'var x = async\ny => y': {
        'body.length': 2,
      },
    },
    'Class Method Kinds': {
      'class Kind { foo() {} }': {
        'body.0.body.body.0.kind': 'method',
      },
      'class Kind { "foo"() {} }': {
        'body.0.body.body.0.kind': 'method',
      },
      'class Kind { constructor() {} }': {
        'body.0.body.body.0.kind': 'constructor',
      },
      'class Kind { "constructor"() {} }': {
        'body.0.body.body.0.kind': 'constructor',
      },
      'class Kind { get a() {} }': {
        'body.0.body.body.0.kind': 'get',
      },
      'class Kind { get "a"() {} }': {
        'body.0.body.body.0.kind': 'get',
      },
      'class Kind { set a(x) {} }': {
        'body.0.body.body.0.kind': 'set',
      },
      'class Kind { set "a"(x) {} }': {
        'body.0.body.body.0.kind': 'set',
      },
    },
    'Class Properties': {
      'class Properties { x; }': {
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation': null,
          'value': null,
          'static': false,
          'computed': false,
        }]
      },
      'class Properties { x: string; }': {
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'value': null,
          'static': false,
          'computed': false,
        }]
      },
      'class Properties { x = "hello"; }': {
        '%parse_options%': {
          'esproposal_class_instance_fields': true,
        },
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation': null,
          'value.value': "hello",
          'static': false,
          'computed': false,
        }]
      },
      'class Properties { x = "hello"; }': {
        '%parse_options%': {
          'esproposal_class_instance_fields': false,
        },
        'errors': {
          '0.message': 'Unexpected string',
        },
      },
      'class Properties { x: string = "hello"; }': {
        '%parse_options%': {
          'esproposal_class_instance_fields': true,
        },
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'value.value': "hello",
          'static': false,
          'computed': false,
        }]
      },
      'class Properties { static x; }': {
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation': null,
          'value': null,
          'static': true,
          'computed': false,
        }]
      },
      'class Properties { static x: string; }': {
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'value': null,
          'static': true,
          'computed': false,
        }]
      },
      'class Properties { static x = "hello"; }': {
        '%parse_options%': {
          'esproposal_class_static_fields': true,
        },
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation': null,
          'value.value': "hello",
          'static': true,
          'computed': false,
        }]
      },
      'class Properties { static x = "hello"; }': {
        '%parse_options%': {
          'esproposal_class_instance_fields': false,
        },
        'errors': {
          '0.message': 'Unexpected string',
        },
      },
      'class Properties { static x: string = "hello"; }': {
        '%parse_options%': {
          'esproposal_class_static_fields': true,
        },
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'value.value': "hello",
          'static': true,
          'computed': false,
        }]
      },
      'class Properties { static [x]: string = "hello"; }': {
        '%parse_options%': {
          'esproposal_class_static_fields': true,
        },
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'value.value': "hello",
          'static': true,
          'computed': true,
        }]
      },
      'class Properties { get; }': {
        'body.0.body.body.0.key.name': 'get',
      },
      'class Properties { get: string; }': {
        'body.0.body.body.0.key.name': 'get',
      },
      'class Properties { get = 123; }': {
        '%parse_options%': {
          'esproposal_class_instance_fields': true,
        },
        'body.0.body.body.0.key.name': 'get',
      },
      'class Properties { get<T>() {} }': {
        'body.0.body.body.0.key.name': 'get',
      },
      'class Properties { get() {} }': {
        'body.0.body.body.0.key.name': 'get',
      },
      'class Properties { set; }': {
        'body.0.body.body.0.key.name': 'set',
      },
      'class Properties { set: string; }': {
        'body.0.body.body.0.key.name': 'set',
      },
      'class Properties { set = 123; }': {
        '%parse_options%': {
          'esproposal_class_instance_fields': true,
        },
        'body.0.body.body.0.key.name': 'set',
      },
      'class Properties { set<T>() {} }': {
        'body.0.body.body.0.key.name': 'set',
      },
      'class Properties { set() {} }': {
        'body.0.body.body.0.key.name': 'set',
      },
    },
    'Comments': {
      // Regression test: "/*" should be allowed inside block comments
      '/* /* */': {
        'comments': [{
          'type': 'Block',
          'value': ' /* ',
        }]
      },
    },
    'Decorators (experimental/early)': {
      'class Foo { @myDecorator1 @myDecorator2 myMethod() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.body.body': [{
          'type': 'MethodDefinition',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator1'},
            {'type': 'Identifier', 'name': 'myDecorator2'},
          ]
        }]
      },
      'class Foo { @myDecorator1 @myDecorator2 *myMethod() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.body.body': [{
          'type': 'MethodDefinition',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator1'},
            {'type': 'Identifier', 'name': 'myDecorator2'},
          ]
        }]
      },
      'class Foo { @myDecorator1 @myDecorator2 static myMethod() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.body.body': [{
          'type': 'MethodDefinition',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator1'},
            {'type': 'Identifier', 'name': 'myDecorator2'},
          ]
        }]
      },
      'class Foo { @myDecorator1 @myDecorator2 async myMethod() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.body.body': [{
          'type': 'MethodDefinition',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator1'},
            {'type': 'Identifier', 'name': 'myDecorator2'},
          ]
        }]
      },
      'class Foo { @myDecorator1 @myDecorator2 get myProp() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.body.body': [{
          'type': 'MethodDefinition',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator1'},
            {'type': 'Identifier', 'name': 'myDecorator2'},
          ]
        }]
      },
      'class Foo { @myDecorator1 @myDecorator2 set myProp(v) {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.body.body': [{
          'type': 'MethodDefinition',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator1'},
            {'type': 'Identifier', 'name': 'myDecorator2'},
          ]
        }]
      },
      'class Foo { @myDecorator("someParam") myMethod() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.body.body': [{
          'type': 'MethodDefinition',
          'decorators': [
            {
              'type': 'CallExpression',
              'callee': {
                'type': 'Identifier',
                'name': 'myDecorator'
              },
              'arguments': [{
                'type': 'Literal',
                'value': 'someParam'
              }]
            }
          ]
        }]
      }
    },
    'Invalid Decorators': {
      '@blah': {
        'errors': {
          '0.message': 'Unexpected token @',
        },
      }
    },
    'Valid uninitialized destructured bindings': {
      'function x({ foo }) {}': {},
      'var x = function({ foo }) {}': {},
      'var x = ({ foo }) => {}': {},
      'for (var { foo } of xs) {}': {},
      'for (let { foo } of xs) {}': {},
      'for (const { foo } of xs) {}': {},
      'for (var { foo } in xs) {}': {},
      'for (let { foo } in xs) {}': {},
      'for (const { foo } in xs) {}': {}
    },
    'Invalid uninitialized destructured bindings': {
      'for (var { foo };;) {}': {
        'errors': {
          '0': {
            'message': 'Destructuring assignment must be initialized',
            'loc.start.column': 9,
            'loc.end.column': 16
          }
        }
      },
      'for (let { foo };;) {}': {
        'errors': {
          '0': {
            'message': 'Destructuring assignment must be initialized',
            'loc.start.column': 9,
            'loc.end.column': 16
          }
        }
      },
      'for (const { foo };;) {}': {
        'errors': {
          '0': {
            'message': 'Destructuring assignment must be initialized',
            'loc.start.column': 11,
            'loc.end.column': 18
          },
          '1': {
            'message': 'Const must be initialized',
            'loc.start.column': 11,
            'loc.end.column': 18
          }
        }
      }
    },
    'Valid uninitialized const bindings': {
      'for (const x in xs) {}': {},
      'for (const x of xs) {}': {}
    },
    'Invalid uninitialized const bindings': {
      'for (const x;;) {}': {
        'errors': {
          '0': {
            'message': 'Const must be initialized',
            'loc.start.column': 11,
            'loc.end.column': 12
          }
        }
      }
    },
    'Valid export/import reserved words': {
      'export { default } from "foo"': {},
      'export { default as foo } from "foo"': {},
      'export { foo as default } from "foo"': {},
      'export { foo as default }': {},
      'import { default as foo } from "foo"': {}
    },
    'Invalid export/import reserved words': {
      'export { default }': {
        'errors.0.message': 'Unexpected token default'
      },
      'import { default } from "foo"': {
        'errors.0.message': 'Unexpected token default'
      },
      'import { foo as default } from "foo"': {
        'errors.0.message': 'Unexpected token default'
      }
    },
    'Valid yield expressions': {
      'yield foo;': {},
      'yield* foo;': {},
      'yield;': {
        'body.0.expression.argument': null
      },
      'yield\nfoo': {
        'body.0.expression.argument': null
      },
      '{ yield }': {
        'body.0.body.0.expression.argument': null
      }
    },
    'Invalid yield expressions': {
      'yield*;': {
        'errors.0.message': 'Unexpected token ;'
      }
    }
  }
};
