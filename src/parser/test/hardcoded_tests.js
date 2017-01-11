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
            'type': 'TypeParameter',
            'name': 'T',
          }
        ]
      },
      'function foo<T,S>() {}': {
        'body.0.typeParameters.params': [
          {
            'type': 'TypeParameter',
            'name': 'T',
          },
          {
            'type': 'TypeParameter',
            'name': 'S',
          }
        ]
      },
      'function foo(...typedRest: Array<number>){}': {
        'body.0.params.0': {
          'type': 'RestElement',
          'argument': {
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
        }
      },
      'a=function<T,S>() {}': {
        'body.0.expression.right.typeParameters.params': [
          {
            'type': 'TypeParameter',
            'name': 'T',
          },
          {
            'type': 'TypeParameter',
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
              ],
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
                  'type': 'TypeParameter',
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
      'var a: { get foo(): number; }': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation.properties': [
          {
            'key.name': 'foo',
            'value': {
              'type': 'FunctionTypeAnnotation',
              'params': [],
              'returnType.type': 'NumberTypeAnnotation',
              'rest': null,
            },
            'kind': 'get',
          }
        ]
      },
      'var a: { set foo(x: number): void; }': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation.properties': [
          {
            'key.name': 'foo',
            'value': {
              'type': 'FunctionTypeAnnotation',
              'params': [
                {
                  'name.name': 'x',
                  'typeAnnotation.type': 'NumberTypeAnnotation',
                }
              ],
              'returnType.type': 'VoidTypeAnnotation',
              'rest': null,
            },
            'kind': 'set',
          }
        ]
      },
      'var a: { get foo(): number; set foo(x: number): void; }': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation.properties': [
          {
            'key.name': 'foo',
            'value': {
              'type': 'FunctionTypeAnnotation',
              'params': [],
              'returnType.type': 'NumberTypeAnnotation',
              'rest': null,
            },
            'kind': 'get',
          },
          {
            'key.name': 'foo',
            'value': {
              'type': 'FunctionTypeAnnotation',
              'params': [
                {
                  'name.name': 'x',
                  'typeAnnotation.type': 'NumberTypeAnnotation',
                }
              ],
              'returnType.type': 'VoidTypeAnnotation',
              'rest': null,
            },
            'kind': 'set',
          }
        ]
      },
      'var a: {get?: number; set?: string}': {
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'properties.0.optional': true,
          'properties.1.optional': true,
        }
      },
      'var a: { get foo(x: string): number; }': {
        'errors': {
          '0': {
            'message': 'Getter should have zero parameters',
            'loc.start': { 'line': 1, 'column': 13 },
            'loc.end': { 'line': 1, 'column': 16 },
          }
        }
      },
      'var a: { set foo(): void; }': {
        'errors': {
          '0': {
            'message': 'Setter should have exactly one parameter',
            'loc.start': { 'line': 1, 'column': 13 },
            'loc.end': { 'line': 1, 'column': 16 },
          }
        }
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
            'type': 'AssignmentPattern',
            'left.optional': true,
            'right.value': 123,
          },
        ],
      },
      'class Foo {set fooProp(value:number){}}': {
        'body.0.body.body.0.value.params.0.typeAnnotation.typeAnnotation': {
          'type': 'NumberTypeAnnotation',
        },
      },
      'a = class Foo<T> { }': {
        'body.0.expression.right.typeParameters.params': [
          {
            'type': 'TypeParameter',
            'name': 'T',
          }
        ]
      },
      'class Foo<T> {}': {
        'body.0.typeParameters.params': [
          {
            'type': 'TypeParameter',
            'name': 'T',
          }
        ]
      },
      'class Foo<T> { bar<U>():number { return 42; }}': {
        'body.0.body.body.0.value': {
          'typeParameters.params': [
            {
              'type': 'TypeParameter',
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
              'type': 'TypeParameter',
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
            'type': 'TypeParameter',
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
            'type': 'TypeParameter',
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
      'class Foo<+T1,-T2> {}': {
        'body.0.typeParameters.params': [
          {
            'type': 'TypeParameter',
            'name': 'T1',
            'variance': 'plus',
          },
          {
            'type': 'TypeParameter',
            'name': 'T2',
            'variance': 'minus',
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
      'type foo<A,B,> = bar;': {
        'body.0.typeParameters.params': [
          {'type': 'TypeParameter', 'name': 'A'},
          {'type': 'TypeParameter', 'name': 'B'},
        ]
      },
      'class Foo<A,B,> extends Bar<C,D,> {}': {
        'body.0.typeParameters.params': [
          { 'type': 'TypeParameter', 'name': 'A' },
          { 'type': 'TypeParameter', 'name': 'B' },
        ],
        'body.0.superTypeParameters.params': [
          { 'type': 'GenericTypeAnnotation', 'id.name': 'C' },
          { 'type': 'GenericTypeAnnotation', 'id.name': 'D' },
        ],
      },
      'interface Foo<A,B,> {}': {
        'body.0.typeParameters.params': [
          {'type': 'TypeParameter', 'name': 'A'},
          {'type': 'TypeParameter', 'name': 'B'},
        ]
      },
      'function f<A,B,>() {}': {
        'body.0.typeParameters.params': [
          {'type': 'TypeParameter', 'name': 'A'},
          {'type': 'TypeParameter', 'name': 'B'},
        ]
      },
      'type Foo = Array<*>': {
        'body.0.right.typeParameters.params': [
          {'type': 'ExistsTypeAnnotation'}
        ]
      },
      'type T = { a: | number | string }': {
        'body.0.right.properties.0.value': {
          'type': 'UnionTypeAnnotation',
          'types': [
            {'type': 'NumberTypeAnnotation'},
            {'type': 'StringTypeAnnotation'},
          ]
        }
      }
    },
    'Exact object types': {
      'var obj: {| x: number, y: string |}': {  // no trailing comma
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'ObjectTypeAnnotation',
          'exact': true,
        },
      },
      'var obj: {| x: number, y: string, |}': {  // trailing comma
        'body.0.declarations.0.id.typeAnnotation.typeAnnotation': {
          'type': 'ObjectTypeAnnotation',
          'exact': true,
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
                'type': 'TypeParameter',
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
      'type union = | A | B | C': {
        'body': [
          {
            'type': 'TypeAlias',
            'id.name': 'union',
            'right': {
              'type': 'UnionTypeAnnotation',
              'types': [
                {'type': 'GenericTypeAnnotation', 'id.name': 'A'},
                {'type': 'GenericTypeAnnotation', 'id.name': 'B'},
                {'type': 'GenericTypeAnnotation', 'id.name': 'C'},
              ]
            },
          },
        ],
      },
      'type overloads = & ((x: string) => number) & ((x: number) => string);': {
        'body': [
          {
            'type': 'TypeAlias',
            'id.name': 'overloads',
            'right': {
              'type': 'IntersectionTypeAnnotation',
              'types': [
                {'type': 'FunctionTypeAnnotation'},
                {'type': 'FunctionTypeAnnotation'}
              ]
            },
          },
        ],
      }
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
              'type': 'TypeParameter',
              'name': 'T',
            },
            {
              'type': 'TypeParameter',
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
              'type': 'TypeParameter',
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
        'errors': [
          {'message': 'When exporting a class as a named export, you must specify a class name. Did you mean `export default class ...`?'}
        ]
      },
      'export class Foo {}': {
        'body.0.declaration': {
          'type': 'ClassDeclaration'
        }
      },
      'export default class {}': {
        'body.0.declaration': {
          'type': 'ClassDeclaration',
        }
      },
      'export default class A {}': {
        'body.0.declaration': {
          'type': 'ClassDeclaration',
        }
      },
      'export default function() {}': {
        'body.0.declaration': {
          'type': 'FunctionDeclaration',
        }
      },
      'export default function a() {}': {
        'body.0.declaration': {
          'type': 'FunctionDeclaration',
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
          'type': 'ExportNamespaceSpecifier',
          'exported': {
            'type': 'Identifier',
            'name': 'foo'
          },
        }]
      },

      // Duplicate exports are an early/parse error
      'export let foo = 1; export const foo = 2;': {
        'errors': [
          {'message': 'Duplicate export for `foo`'}
        ]
      },
      'export const foo = 1; export var foo = 2;': {
        'errors': [
          {'message': 'Duplicate export for `foo`'}
        ]
      },
      'export var foo = 1; export let foo = 2;': {
        'errors': [
          {'message': 'Duplicate export for `foo`'}
        ]
      },
      'export function foo() {}; export var foo = 1;': {
        'errors': [
          {'message': 'Duplicate export for `foo`'}
        ]
      },
      'export function foo() {}; export async function foo() {};': {
        'errors': [
          {'message': 'Duplicate export for `foo`'}
        ]
      },
      'export function foo() {}; export class foo {};': {
        'errors': [
          {'message': 'Duplicate export for `foo`'}
        ]
      },
      'export {foo}; export {foo};': {
        'errors': [
          {'message': 'Duplicate export for `foo`'}
        ]
      },
      'export {foo as bar}; export {bar};': {
        'errors': [
          {'message': 'Duplicate export for `bar`'}
        ]
      },
      'export default 42; export default 43;': {
        'errors': [
          {'message': 'Duplicate export for `default`'}
        ]
      },
      'export function foo() { var foo = 42; }': {
        'errors': []
      },

      // Not errors (but easily mistaken as one via bug)
      'export {foo as bar}; export {foo};': {
        'errors': []
      },
      'export default (foo = 42); export let foo = 43;': {
        'errors': []
      },
      'export let bar = 43; export let foo = (bar = 43)': {
        'errors': []
      },
      'export default function<T>(arg:T):T {return arg;}': {
        'errors': [],
        'body.0.declaration.typeParameters.params': [
          {'type': 'TypeParameter', 'name': 'T'}
        ]
      },
      'export default class<T>{}': {
        'errors': [],
        'body.0.declaration.typeParameters.params': [
          {'type': 'TypeParameter', 'name': 'T'}
        ]
      }
    },
    'ES6: new.target': {
        'new.target': {
          'errors': [
            {'message': 'Unexpected token .'}
          ],
        },
        'var x = function() { y = new..target; }': {
          'errors.0': {'message': 'Unexpected token .', 'loc.start.column': 29},
        },
        'function x() { new.unknown_property; }': {
          'errors.0': {
            'message': 'Unexpected identifier',
            'loc.start.column': 19,
            'loc.end.column': 35,
          },
        },
        'function x() { new.target }': {
          'body.0.body.body.0.expression': {
            'type': 'MetaProperty',
            'meta': {'type': 'Identifier', 'name': 'new'},
            'property': {'type': 'Identifier', 'name': 'target'},
            'loc.start.column': 15,
            'loc.end.column': 25,
          },
        },
        'function x() { let x = new.target; }': {
          'body.0.body.body.0.declarations.0.init': {
            'type': 'MetaProperty',
            'meta': {'type': 'Identifier', 'name': 'new'},
            'property': {'type': 'Identifier', 'name': 'target'},
            'loc.start.column': 23,
            'loc.end.column': 33,
          }
        },
        'function x() { new new.target; }': {
          'body.0.body.body.0.expression': {
            'type': 'NewExpression',
            'callee': {
              'type': 'MetaProperty',
              'meta': {'type': 'Identifier', 'name': 'new'},
              'property': {'type': 'Identifier', 'name': 'target'},
              'loc.start.column': 19,
              'loc.end.column': 29,
            }
          },
        },
        'function x() { new.target(); }': {
          'body.0.body.body.0.expression': {
            'type': 'CallExpression',
            'callee': {
              'type': 'MetaProperty',
              'meta': {'type': 'Identifier', 'name': 'new'},
              'property': {'type': 'Identifier', 'name': 'target'},
              'loc.start.column': 15,
              'loc.end.column': 25,
            },
            'arguments': [],
          }
        },
        'function x() { new new.target()(); }': {
          'body.0.body.body.0.expression': {
            'type': 'CallExpression',
            'callee': {
              'type': 'NewExpression',
              'callee': {
                'type': 'MetaProperty',
                'meta': {'type': 'Identifier', 'name': 'new'},
                'property': {'type': 'Identifier', 'name': 'target'},
                'loc.start.column': 19,
                'loc.end.column': 29,
              },
              'arguments': [],
            },
            'arguments': [],
          }
        },
    },
    'ES6: object short notation': {
      'let get = 123; let x = { get };': {
        'body.1.declarations.0.init.properties.0': {
          'key': { 'type': 'Identifier', 'name': 'get' },
          'value': { 'type': 'Identifier', 'name': 'get' },
        }
      },
      'let set = 123; let x = { set };': {
        'body.1.declarations.0.init.properties.0': {
          'key': { 'type': 'Identifier', 'name': 'set' },
          'value': { 'type': 'Identifier', 'name': 'set' },
        }
      },
      'let get = 123, set = 234; let x = { get, set };': {
        'body.1.declarations.0.init.properties.0': {
          'key': { 'type': 'Identifier', 'name': 'get' },
          'value': { 'type': 'Identifier', 'name': 'get' },
        },
        'body.1.declarations.0.init.properties.1': {
          'key': { 'type': 'Identifier', 'name': 'set' },
          'value': { 'type': 'Identifier', 'name': 'set' },
        }
      },
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
          'predicate': null,
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
          'predicate': null,
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
          'predicate': null,
        },
      },
      // Function argument types are now optional
      'declare function foo(x: number, string): void': {
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
                  'typeAnnotation.type': 'StringTypeAnnotation',
                },
              ],
              'returnType.type': 'VoidTypeAnnotation',
            },
          },
          'predicate': null,
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
      'declare class A { get foo(): number; }': {
        'body.0.body.properties.0': {
          'static': false,
          'kind': 'get',
        }
      },
      'declare class A { set foo(x: number): void; }': {
        'body.0.body.properties.0': {
          'static': false,
          'kind': 'set',
        }
      },
      'declare class A { get foo(): number; set foo(x: string): void; }': {
        'body.0.body.properties.0.kind': 'get',
        'body.0.body.properties.1.kind': 'set'
      },
    },
    'Declare Export': {
      // Batch export
      'declare export * from "foo";': {
        'body': [
          {
            'type': 'DeclareExportAllDeclaration',
            'source.value': "foo",
          },
        ],
      },
      'declare export * from "foo"': {
        'body': [
          {
            'type': 'DeclareExportAllDeclaration',
            'source.value': "foo",
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
            'local.name': 'bar',
          },
        ],
      },
      'declare export { bar } from "foo"': {},
      'declare export { bar, baz } from "foo";': {
        'body.0.specifiers': [
          {
            'local.name': 'bar',
          },
          {
            'local.name': 'baz',
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
          'predicate': null,
        },
      },
      'declare export function foo(): void;': {},
      'declare export function foo<T>(): void;': {},
      'declare export function foo(x: number, y: string): void;': {},
      // Function argument types are now optional
      'declare export function foo(x: number, string): void': {},

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
          'predicate': null,
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
    'Declare module.exports': {
      'declare module.exports: number': {
        'body.0.type': 'DeclareModuleExports',
        'body.0.typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
      },
      'declare module "foo" { declare module.exports: number; }': {
        'body.0.body.body.0': {
          'type': 'DeclareModuleExports',
          'typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
        },
      },
    },
    '`declare module {}` with exports': {
      'declare module "foo" { declare export * from "bar"; }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.0': {
            'type': 'DeclareExportAllDeclaration',
            'source': {
              'type': 'Literal',
              'value': 'bar',
            },
          },
          'kind': 'ES',
        },
      },

      'declare module "foo" { declare export {a,} from "bar"; }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.0': {
            'type': 'DeclareExportDeclaration',
            'declaration': null,
            'specifiers': [{
              'type': 'ExportSpecifier',
              'local': {
                'type': 'Identifier',
                'name': 'a',
                'typeAnnotation': null,
              },
              'exported': {
                'type': 'Identifier',
                'name': 'a',
                'typeAnnotation': null,
              },
            }],
            'source': {
              'type': 'Literal',
              'value': 'bar',
            },
          },
          'kind': 'ES',
        },
      },

      'declare module "foo" { declare export {a,}; }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.0': {
            'type': 'DeclareExportDeclaration',
            'declaration': null,
            'specifiers': [{
              'type': 'ExportSpecifier',
              'local': {
                'type': 'Identifier',
                'name': 'a',
                'typeAnnotation': null,
              },
              'exported': {
                'type': 'Identifier',
                'name': 'a',
                'typeAnnotation': null,
              },
            }],
            'source': null,
          },
          'kind': 'ES',
        },
      },

      'declare module "foo" { declare export var a: number; }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.0': {
            'type': 'DeclareExportDeclaration',
            'declaration.id': {
              'type': 'Identifier',
              'name': 'a',
              'typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
            },
            'specifiers.length': 0,
            'source': null,
          },
        },
      },

      'declare module "foo" { declare export function bar(p1: number): string; }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.0': {
            'type': 'DeclareExportDeclaration',
            'declaration': {
              'type': 'DeclareFunction',
              'id': {
                'name': 'bar',
                'typeAnnotation.typeAnnotation': {
                  'type': 'FunctionTypeAnnotation',
                  'params': [{
                    'type': 'FunctionTypeParam',
                    'name.name': 'p1',
                    'typeAnnotation.type': 'NumberTypeAnnotation',
                  }],
                  'returnType.type': 'StringTypeAnnotation',
                },
              },
              'predicate': null,
            },
            'specifiers.length': 0,
            'source': null,
          },
          'kind': 'ES',
        },
      },

      'declare module "foo" { declare export class Foo { meth(p1: number): void; } }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.0': {
            'type': 'DeclareExportDeclaration',
            'declaration': {
              'type': 'DeclareClass',
              'id': {'name': 'Foo', 'typeAnnotation': null},
              'body.properties': [{
                'type': 'ObjectTypeProperty',
                'key': {'type': 'Identifier', 'name': 'meth'},
                'value': {
                  'type': 'FunctionTypeAnnotation',
                  'params': [{
                    'type': 'FunctionTypeParam',
                    'name': {'type': 'Identifier', 'name': 'p1'},
                    'typeAnnotation.type': 'NumberTypeAnnotation',
                  }],
                  'returnType.type': 'VoidTypeAnnotation',
                },
              }],
            },
            'specifiers.length': 0,
            'source': null,
          },
          'kind': 'ES',
        },
      },

      'declare module "foo" { declare export type bar = number; }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.0': {
            'type': 'DeclareExportDeclaration',
            'declaration': {
              'type': 'TypeAlias',
              'id': {'type': 'Identifier', 'name': 'bar'},
              'right.type': 'NumberTypeAnnotation',
            },
            'specifiers.length': 0,
            'source': null,
          },
          'kind': 'CommonJS',
        },
      },

      'declare module "foo" { declare export type bar = number; declare export var baz: number; }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.length': 2,
          'kind': 'ES',
        },
      },

      'declare module "foo" { declare export type bar = number; declare module.exports: number; }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.length': 2,
          'kind': 'CommonJS',
        },
      },

      'declare module "foo" { declare export interface bar {} }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.0': {
            'type': 'DeclareExportDeclaration',
            'declaration': {
              'type': 'InterfaceDeclaration',
              'id': {'type': 'Identifier', 'name': 'bar'},
            },
            'specifiers.length': 0,
            'source': null,
          },
          'kind': 'CommonJS',
        },
      },

      'declare module "foo" { declare export interface bar {} declare export var baz: number; }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.length': 2,
          'kind': 'ES',
        },
      },

      'declare module "foo" { declare export interface bar {} declare module.exports: number; }': {
        'body.0': {
          'type': 'DeclareModule',
          'body.body.length': 2,
          'kind': 'CommonJS',
        },
      },
    },
    'Invalid Declare Export': {
      // declare export type is not supported at the toplevel since export type
      // is identical there.
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
    'Invalid declare module.exports': {
      'declare module.exports': {
        'errors.0.message': 'Unexpected end of input',
      },
      'declare module "foo" { declare module.exports: number; declare module.exports: string; }': {
        'errors.0.message': 'Duplicate `declare module.exports` statement!',
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
        'errors': [
          {
            'message': 'Unexpected token ILLEGAL',
            'loc.start.column': 16,
            'loc.end.column': 17,
          },
          {
            'message': 'Unexpected token ILLEGAL',
            'loc.start.column': 17,
            'loc.end.column': 18,
          },
          {
            'message': 'Unexpected identifier',
            'loc.start.column': 18,
            'loc.end.column': 23,
          },
        ]
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
    'JSX Syntax': {
      '(<div>{}</div>)': {
        'body.0.expression.children': [
          {
            'type': 'JSXExpressionContainer',
            'loc.start.column': 6,
            'loc.end.column': 8,
            'range': [6, 8],
            'expression': {
              'type': 'JSXEmptyExpression',
              'loc.start.column': 7,
              'loc.end.column': 7,
              'range': [7, 7]
            }
          }
        ]
      },
      '(<div>{ }</div>)': {
        'body.0.expression.children': [
          {
            'type': 'JSXExpressionContainer',
            'loc.start.column': 6,
            'loc.end.column': 9,
            'range': [6, 9],
            'expression': {
              'type': 'JSXEmptyExpression',
              'loc.start.column': 7,
              'loc.end.column': 8,
              'range': [7, 8]
            }
          }
        ]
      },
      '(<div>{\n\n}</div>)': {
        'body.0.expression.children': [
          {
            'type': 'JSXExpressionContainer',
            'loc.start.column': 6,
            'loc.end.column': 1,
            'range': [6, 10],
            'expression': {
              'type': 'JSXEmptyExpression',
              'loc.start.column': 7,
              'loc.end.column': 0,
              'range': [7, 9]
            }
          }
        ]
      }
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
      'var a: (number: number) => number = (number) => { return 123; }': {}
    },
    'Invalid Type Grouping': {
      'var a: (true: number) => number = (number) => { return 123; }': {
        'errors.0': {
          'message': 'Unexpected token :',
          'loc.start.line': 1,
          'loc.start.column': 12,
          'loc.end.line': 1,
          'loc.end.column': 13
        }
      }
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
    'Exponentiation Operator': {
      '2 ** 3;': {
        'body.0.expression': {
          'type': 'BinaryExpression',
          'range': [0,6],
          'operator': '**',
          'left': {'type':'Literal', 'range':[0,1], 'value':2},
          'right': {'type':'Literal', 'range':[5,6],'value':3}
        },
        'errors': []
      },
      // ** is right-associative: (1 + (2 ** (3 ** 4))) + 5
      '1 + 2 ** 3 ** 4 + 5;': {
        'body.0.expression': {
          'type': 'BinaryExpression',
          'range': [0,19],
          'operator': '+',
          'left': {
            'type': 'BinaryExpression',
            'range': [0,15],
            'operator': '+',
            'left.value': 1,
            'right': {
              'type': 'BinaryExpression',
              'range': [4,15],
              'operator': '**',
              'left.value': 2,
              'right': {
                'type': 'BinaryExpression',
                'range': [9,15],
                'operator': '**',
                'left.value': 3,
                'right.value': 4,
              }
            }
          },
          'right': {'type': 'Literal', 'range': [18,19], 'value': 5}
        },
        'errors': []
      },
      '(-1) ** 2': {
        'body.0.expression': {
          'type': 'BinaryExpression',
          'range': [0,9],
          'operator': '**',
          'left': {
            'type': 'UnaryExpression',
            'range': [1,3],
            'operator': '-',
            'prefix': true,
            'argument.value': 1,
          },
          'right.value': 2,
        },
        'errors': []
      },
      '-1 ** 2': {
        'body.0.expression': {
          'type': 'BinaryExpression',
          'range': [0,7],
          'operator': '**',
          'left': {
            'type': 'UnaryExpression',
            'range': [0,2],
            'operator': '-',
            'prefix': true,
            'argument.value': 1,
          },
          'right.value': 2
        },
        'errors': [
          {
            'loc.start.column': 0,
            'loc.end.column': 2,
            'message': 'Invalid left-hand side in exponentiation expression'
          },
        ],
      },
      'let x = 2; x **= 3;': {
        'body.1.expression': {
          'type': 'AssignmentExpression',
          'range': [11,18],
          'operator': '**=',
          'left.name': 'x',
          'right.value': 3
        },
        'errors': [],
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
      'declare module "./a/b.js" {}': {
        'body.0.id': {
          'type': 'Literal',
          'value': './a/b.js',
        }
      },
      'declare module "M" { import type T from "TM"; }': {
        'body.0.body.body.0': {
          'type': 'ImportDeclaration',
          'specifiers': [{
            'type': 'ImportDefaultSpecifier',
            'local': {
              'type': 'Identifier',
              'name': 'T',
            }
          }],
          'source': {
            'type': 'Literal',
            'value': 'TM',
          },
        }
      },
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
      'declare module "foo" { declare export var a: number; declare module.exports: number; }': {
        'errors': {
          '0.message': 'Found both `declare module.exports` and `declare export` in the same module. Modules can only have 1 since they are either an ES module xor they are a CommonJS module.'
        }
      },
      'declare module "M" { import T from "TM"; }': {
        'errors': {
          '0.message': 'Imports within a `declare module` body must always be `import type` or `import typeof`!'
        }
      }
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
            'type': 'TypeParameter',
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
          '0.message': 'Unexpected token *',
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
    'Type Annotations in Comments With Types Disabled': {
      'function foo(numVal/*  : number */){}': {
        '%parse_options%': {
          'types': false,
        },
        'body.0.params.0': {
          'name': 'numVal',
          'typeAnnotation': null,
          'loc.start.column': 13,
          'loc.end.column': 19
        },
        'comments': [{
          'type': 'Block',
          'value': '  : number ',
          'loc.start.column': 19,
          'loc.end.column': 34
        }]
      },
      'type Foo = /*:: number */': {
        '%parse_options%': {
          'types': false,
        },
        'errors': [
          {
            'message': 'Type aliases are not allowed in untyped mode',
            'loc.start.line': 1,
            'loc.start.column': 0,
          },
          {
            'message': 'Unexpected end of input'
          }
        ],
        'comments': [{
          'type': 'Block',
          'value': ':: number ',
          'loc.start.column': 11,
          'loc.end.column': 25
        }]
      }
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
      'class async { async: number; }': {},
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
    'Async Generators': {
      'async function *foo() {}' : {
        'errors': [],
        'body.0': {
          'type': 'FunctionDeclaration',
          'async': true,
          'generator': true,
        },
      },
      'async function *ft<T>(a: T) {}' : {
        'errors': [],
        'body.0': {
          'type': 'FunctionDeclaration',
          'async': true,
          'generator': true,
        },
      },
      'class C { async *m() {} }' : {
        'errors': [],
        'body.0.body.body.0': {
          'type': 'MethodDefinition',
          'value': {
            'type': 'FunctionExpression',
            'async': true,
            'generator': true,
          },
        },
      },
      'class C { async *mt<T>(a: T) {} }' : {
        'errors': [],
        'body.0.body.body.0': {
          'type': 'MethodDefinition',
          'value': {
            'type': 'FunctionExpression',
            'async': true,
            'generator': true,
          },
        },
      },
      'class C { static async *m(a) {} }' : {
        'errors': [],
        'body.0.body.body.0': {
          'type': 'MethodDefinition',
          'value': {
            'type': 'FunctionExpression',
            'async': true,
            'generator': true,
          },
        },
      },
      'class C { static async *mt<T>(a: T) {} }' : {
        'errors': [],
        'body.0.body.body.0': {
          'type': 'MethodDefinition',
          'value': {
            'type': 'FunctionExpression',
            'async': true,
            'generator': true,
          },
        },
      },
      'var e = async function *() {};' : {
        'errors': [],
        'body.0.declarations.0.init': {
          'type': 'FunctionExpression',
          'async': true,
          'generator': true,
        },
      },
      'var et = async function*<T> (a: T) {};' : {
        'errors': [],
        'body.0.declarations.0.init': {
          'type': 'FunctionExpression',
          'async': true,
          'generator': true,
        },
      },
      'var n = new async function*() {};' : {
        'errors': [],
        'body.0.declarations.0.init': {
          'type': 'NewExpression',
          'callee': {
            'type': 'FunctionExpression',
            'async': true,
            'generator': true,
          },
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
      'class Properties { x = "hi mom"; }': {
        '%parse_options%': {
          'esproposal_class_instance_fields': false,
        },
        'errors': {
          '0.message': 'Unexpected token =',
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
      'class Properties { static x = "hi mom"; }': {
        '%parse_options%': {
          'esproposal_class_static_fields': false,
        },
        'errors.0.message': 'Unexpected token =',
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
      'class Properties { x: string y: number; }': {
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'value': null,
          'static': false,
          'computed': false,
        }, {
          'type': 'ClassProperty',
          'key.name': 'y',
          'typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
          'value': null,
          'static': false,
          'computed': false,
        }]
      },
      'class Properties { x: string; [y]: number; }': {
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'value': null,
          'static': false,
          'computed': false,
        }, {
          'type': 'ClassProperty',
          'key.name': 'y',
          'typeAnnotation.typeAnnotation.type': 'NumberTypeAnnotation',
          'value': null,
          'static': false,
          'computed': true,
        }]
      },
      'class X {\n  x = "y"\n  foo() {}\n}': {
        '%parse_options%': {
          'esproposal_class_instance_fields': true,
        },
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'value': { 'type': 'Literal', 'value': 'y'},
          'static': false,
          'computed': false,
        }, {
          'type': 'MethodDefinition',
          'key': {
            'type': 'Identifier',
            'name': 'foo',
          },
          'value.type': 'FunctionExpression',
          'kind': 'method',
        }],
        'errors': [],
      },
      'class X {\n  x = "y"\n  [computed] = "z"\n}': {
        '%parse_options%': {
          'esproposal_class_instance_fields': true,
        },
        'body.0.body.body.0': {
          'type': 'ClassProperty',
          'key.name': 'x',
          'value': {
            'type': 'AssignmentExpression',
            'operator': '=',
            'left': {
              'type': 'MemberExpression',
              'object.value': 'y',
              'property.name': 'computed',
            },
            'right': {
              'type': 'Literal',
              'value': 'z',
            },
          },
          'static': false,
          'computed': false,
        },
        'errors': [],
      },
      'class X {\n  x = "y"\n  [computed]: string\n}': {
        '%parse_options%': {
          'esproposal_class_instance_fields': true,
        },
        'body.0.body.body.0': {
          'type': 'ClassProperty',
          'key.name': 'x',
          'value': {
            'type': 'MemberExpression',
            'object.value': 'y',
            'property.name': 'computed',
          },
          'static': false,
          'computed': false,
        },
        'errors.0': {
          'loc.start.line': 3,
          'loc.start.column': 12,
          'loc.end.line': 3,
          'loc.end.column': 13,
          'message': 'Unexpected token :'
        },
      },
      'class X {\n  x: string\n  [computed]: string\n}': {
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'value': null,
          'static': false,
          'computed': false,
        }, {
          'type': 'ClassProperty',
          'key.name': 'computed',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'value': null,
          'static': false,
          'computed': true,
        }],
        'errors': [{
          'loc.start.line': 3,
          'loc.start.column': 2,
          'loc.end.line': 3,
          'loc.end.column': 3,
          'message': 'Unexpected token ['
        }],
      },
      'class X {\n  x: string\n  foo(): void {}\n}': {
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'typeAnnotation.typeAnnotation.type': 'StringTypeAnnotation',
          'value': null,
          'static': false,
          'computed': false,
        }, {
          'type': 'MethodDefinition',
          'key': {
            'type': 'Identifier',
            'name': 'foo',
          },
          'value.type': 'FunctionExpression',
          'kind': 'method',
        }],
        'errors': [],
      },
      'class X {\n  x = () => {}\n  y(): void {}\n}': {
        '%parse_options%': {
          'esproposal_class_instance_fields': true,
        },
        'body.0.body.body': [{
          'type': 'ClassProperty',
          'key.name': 'x',
          'value.type': 'ArrowFunctionExpression',
        }, {
          'type': 'MethodDefinition',
          'key.name': 'y',
          'value.type': 'FunctionExpression',
          'kind': 'method',
        }],
      },
      'class X {x?:T} {}': {
        'errors': [{
          'loc.start.column': 10,
          'loc.end.column': 11,
          'message': 'Unexpected token ?',
        }]
      },
      'class X {+x?:T} {}': {
        'errors': [{
          'loc.start.column': 11,
          'loc.end.column': 12,
          'message': 'Unexpected token ?',
        }]
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
      // Regression test: there are no comments here!
      'let x = /x/* 5 */y/;': {
        'comments': [],
        'body.0.declarations.0.init': {
          'type': 'BinaryExpression',
          'operator': '*',
          'left': {
            'type': 'BinaryExpression',
            'operator': '*',
            'left': { 'type': 'Literal', 'regex.pattern': 'x' },
            'right': { 'type': 'Literal', 'value': 5 }
          },
          'right': { 'type': 'Literal', 'regex.pattern': 'y' }
        }
      },
      // Regression test: there is a comments here!
      'type Foo = Array<*/* comment */>': {
        'comments': [
          { 'type': 'Block', 'value': ' comment '}
        ],
        'body.0.right.typeParameters.params.0.type': 'ExistsTypeAnnotation'
      }
    },
    'Decorators (experimental/early)': {
      'function Bar() { @myDecorator2 @myDecorator1\nclass Foo { myMethod() {} } }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.body.body.0': {
          'type': 'ClassDeclaration',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator2'},
            {'type': 'Identifier', 'name': 'myDecorator1'},
          ]
        }
      },
      '@myDecorator2 @myDecorator1\nclass Foo { myMethod() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0': {
          'type': 'ClassDeclaration',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator2'},
            {'type': 'Identifier', 'name': 'myDecorator1'},
          ]
        }
      },
      '@myDecorator2 @myDecorator1\nexport class Foo { myMethod() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.declaration': {
          'type': 'ClassDeclaration',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator2'},
            {'type': 'Identifier', 'name': 'myDecorator1'},
          ]
        }
      },
      '@myDecorator1\nexport\n@myDecorator2\nclass Foo { myMethod() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.declaration': {
          'type': 'ClassDeclaration',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator1'},
            {'type': 'Identifier', 'name': 'myDecorator2'},
          ]
        }
      },
      '@myDecorator2 @myDecorator1\nexport default class Foo { myMethod() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.declaration': {
          'type': 'ClassDeclaration',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator2'},
            {'type': 'Identifier', 'name': 'myDecorator1'},
          ]
        }
      },
      '@myDecorator1\nexport default\n@myDecorator2\nclass Foo { myMethod() {} }': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'body.0.declaration': {
          'type': 'ClassDeclaration',
          'decorators': [
            {'type': 'Identifier', 'name': 'myDecorator1'},
            {'type': 'Identifier', 'name': 'myDecorator2'},
          ]
        }
      },
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
      '@BadDirectiveNoParseOption': {
        'errors': {
          '0.message': 'Unexpected token @',
        },
      },
      '@BadDirectiveWithParseOption': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'errors': {
          '0.message': 'Found a decorator in an unsupported position.',
        },
      },
      '@blah\nimport * from "foo"': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'errors': {
          '0.message': 'Found a decorator in an unsupported position.',
        },
      },
      '@blah\ndeclare export class Foo {}': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'errors': {
          '0.message': 'Found a decorator in an unsupported position.',
        },
      },
      '@blah\nlet x = 123;': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'errors': {
          '0.message': 'Found a decorator in an unsupported position.',
        },
      },
      '@blah\nconst x = 123;': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'errors': {
          '0.message': 'Found a decorator in an unsupported position.',
        },
      },
      '@blah\nfunction foo() {}': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'errors': {
          '0.message': 'Found a decorator in an unsupported position.',
        },
      },
      '@blah\ninterface Foo {}': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'errors': {
          '0.message': 'Found a decorator in an unsupported position.',
        },
      },
      '@blah\ndeclare class Foo {}': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'errors': {
          '0.message': 'Found a decorator in an unsupported position.',
        },
      },
      '@blah\ntype Foo = any;': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'errors': {
          '0.message': 'Found a decorator in an unsupported position.',
        },
      },
      '@blah\n10*20;': {
        '%parse_options%': {
          "esproposal_decorators": true
        },
        'errors': {
          '0.message': 'Found a decorator in an unsupported position.',
        },
      },
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
    },
    'Destructuring with default values': {
      'let {x=y} = {}': {
        'body.0.declarations.0.id.properties': [{
          'type': 'Property',
          'key': {
            'type': 'Identifier',
            'name': 'x',
          },
          'value': {
            'type': 'AssignmentPattern',
            'left': {
              'type': 'Identifier',
              'name': 'x',
            },
            'right': {
              'type': 'Identifier',
              'name': 'y',
            }
          },
          'shorthand': true,
          'kind': 'init',
          'method': false,
        }]
      },
      'let {x:y=z} = {}': {
        'errors.length': 0,
        'body.0.declarations.0.id.properties': [{
          'type': 'Property',
          'key': {
            'type': 'Identifier',
            'name': 'x',
          },
          'value': {
            'type': 'AssignmentPattern',
            'left': {
              'type': 'Identifier',
              'name': 'y',
            },
            'right': {
              'type': 'Identifier',
              'name': 'z',
            }
          },
          'shorthand': false,
          'kind': 'init',
          'method': false,
        }]
      },
      'let {p:{q=0,...o}={r:0}} = {p:{r:""}}': {
        'errors.length': 0,
        'body.0.declarations.0.id.properties': [{
          'type': 'Property',
          'key': {
            'type': 'Identifier',
            'name': 'p'
          },
          'value': {
            'type': 'AssignmentPattern',
            'left': {
              'type': 'ObjectPattern',
              'properties': [{
                'type': 'Property',
                'key': {
                  'type': 'Identifier',
                  'name': 'q'
                },
                'value': {
                  'type': 'AssignmentPattern',
                  'left': {
                    'type': 'Identifier',
                    'name': 'q'
                  },
                  'right': {
                    'type': 'Literal',
                    'value':0,
                    'raw': '0'
                  }
                },
                'shorthand':true,
                'kind': 'init',
                'method': false,
              }, {
                'type': 'RestProperty',
                'argument': {
                  'type': 'Identifier',
                  'name': 'o'
                }
              }]
            },
            'right': {'type': 'ObjectExpression'}
          },
          'shorthand':false,
          'kind': 'init',
          'method': false,
        }]
      },
      'let [x=y] = []': {
        'errors.length': 0,
        'body.0.declarations.0.id.elements': [{
          'type': 'AssignmentPattern',
          'left': {
            'type': 'Identifier',
            'name': 'x',
          },
          'right': {
            'type': 'Identifier',
            'name': 'y',
          }
        }]
      }
    },
    'Type Parameter Defaults': {
      'type A<T = string> = T': {},
      'type A<T: ?string = string> = T': {},
      'type A<S, T: ?string = string> = T': {},
      'type A<S = number, T: ?string = string> = T': {},
      'class A<T = string> {}': {},
      'class A<T: ?string = string> {}': {},
      'class A<S, T: ?string = string> {}': {},
      'class A<S = number, T: ?string = string> {}': {},
      '(class A<T = string> {})': {},
      '(class A<T: ?string = string> {})': {},
      '(class A<S, T: ?string = string> {})': {},
      '(class A<S = number, T: ?string = string> {})': {},
      'declare class A<T = string> {}': {},
      'declare class A<T: ?string = string> {}': {},
      'declare class A<S, T: ?string = string> {}': {},
      'declare class A<S = number, T: ?string = string> {}': {},
      'interface A<T = string> {}': {},
      'interface A<T: ?string = string> {}': {},
      'interface A<S, T: ?string = string> {}': {},
      'interface A<S = number, T: ?string = string> {}': {},

      // With defaults, this is now valid
      'var x: Foo<>': {},
      'class A extends B<> {}': {},
    },
    'Invalid Type Parameter Defaults': {
      'type A<T = string : ?string> = T': {
        'errors.0.message': 'Unexpected token :'
      },
      'type A<HasDefault = string, NoDefault> = T': {
        'errors.0.message': 'Type parameter declaration needs a default, since a preceding type parameter declaration has a default.'
      },
      'class A extends B<T = number> {}': {
        'errors.0.message': 'Unexpected token ='
      },
      'var x: Array<T = number>': {
        'errors.0.message': 'Unexpected token ='
      },
      'function foo<T = string>() {}': {
        'errors.0.message': 'Unexpected token ='
      },
      'declare function foo<T = string>() {}': {
        'errors.0.message': 'Unexpected token ='
      },
      '({ foo<T = string>() {} })': {
        'errors.0.message': 'Unexpected token ='
      },
      'class A { foo<T = string>() {} }': {
        'errors.0.message': 'Unexpected token ='
      },
      '(class A { foo<T = string>() {} })': {
        'errors.0.message': 'Unexpected token ='
      },
      'declare class A { foo<T = string>(): void }': {
        'errors.0.message': 'Unexpected token ='
      },
      '<T = string>() => 123': {
        'errors.0.message': 'Unexpected token ='
      }
    },
    'Function Predicates': {
      'declare function f(x: mixed): boolean %checks(x !== null);': {
        'body.0': {
          'type': 'DeclareFunction',
          'id': {
            'name': 'f',
            'typeAnnotation.typeAnnotation': {
              'type': 'FunctionTypeAnnotation',
              'params': [
                {
                  'name.name': 'x',
                  'typeAnnotation.type': 'MixedTypeAnnotation',
                }
              ],
              'returnType.type': 'BooleanTypeAnnotation'
            }
          },
          'predicate':{
            'type':'DeclaredPredicate',
            'value':{
              'type':'BinaryExpression',
              'operator':'!==',
              'left':{
                'type':'Identifier',
                'name':'x',
                'typeAnnotation':null,
                'optional':false
              },
              'right':{
                'type':'Literal',
                'value':null,
                'raw':'null'
              }
            }
          },
        },
      },
      'function foo(x: mixed): %checks { return x !== null }': {
        'body.0': {
          'type':'FunctionDeclaration',
          'id':{
            'type':'Identifier',
            'name':'foo',
            'typeAnnotation':null,
            'optional':false
          },
          'params':[
            {
              'type':'Identifier',
              'name':'x',
              'typeAnnotation':{
                'type':'TypeAnnotation',
                'typeAnnotation':{
                  'type':'MixedTypeAnnotation',
                }
              },
              'optional':false
            }
          ],
          'body':{
            'type':'BlockStatement',
            'body':[
              {
                'type':'ReturnStatement',
                'argument':{
                  'type':'BinaryExpression',
                  'operator':'!==',
                  'left':{
                    'type':'Identifier',
                    'name':'x',
                    'typeAnnotation':null,
                    'optional':false
                  },
                  'right':{
                    'type':'Literal',
                    'value':null,
                    'raw':'null'
                  }
                }
              }
            ]
          },
          'async':false,
          'generator':false,
          'predicate': { 'type': 'InferredPredicate' },
          'expression':false,
          'returnType':null,
          'typeParameters':null
        }
      },
      'var a1 = (x: mixed): %checks => x !== null;': {
        'body.0':     {
          'type':'VariableDeclaration',
          'declarations':[
            {
              'type':'VariableDeclarator',
              'id':{
                'type':'Identifier',
                'name':'a1',
                'typeAnnotation':null,
                'optional':false
              },
              'init':{
                'type':'ArrowFunctionExpression',
                'id':null,
                'params':[
                  {
                    'type':'Identifier',
                    'name':'x',
                    'typeAnnotation':{
                      'type':'TypeAnnotation',
                      'typeAnnotation':{
                        'type':'MixedTypeAnnotation',
                      }
                    },
                    'optional':false
                  }
                ],
                'body':{
                  'type':'BinaryExpression',
                  'operator':'!==',
                  'left':{
                    'type':'Identifier',
                    'name':'x',
                    'typeAnnotation':null,
                    'optional':false
                  },
                  'right':{
                    'type':'Literal',
                    'value':null,
                    'raw':'null'
                  }
                },
                'async':false,
                'generator':false,
                'predicate':{
                  'type':'InferredPredicate',
                },
                'expression':true,
                'returnType':null,
                'typeParameters':null
              }
            }
          ],
          'kind':'var'
        }
      },
      '(x): %checks => x !== null;': {
        'body.0':     {
          'type':'ExpressionStatement',
          'expression':{
            'type':'ArrowFunctionExpression',
            'id':null,
            'params':[
              {
                'type':'Identifier',
                'name':'x',
                'typeAnnotation':null,
                'optional':false
              }
            ],
            'body':{
              'type':'BinaryExpression',
              'operator':'!==',
              'left':{
                'type':'Identifier',
                'name':'x',
                'typeAnnotation':null,
                'optional':false
              },
              'right':{
                'type':'Literal',
                'value':null,
                'raw':'null'
              }
            },
            'async':false,
            'generator':false,
            'predicate':{
              'type':'InferredPredicate',
            },
            'expression':true,
            'returnType':null,
            'typeParameters':null
          }
        }

      },
      'var a3: (x: mixed) => boolean %checks(x !== null);': {
        'errors.0.message': 'Unexpected token %'
      },
      'declare function f(x: mixed): boolean %checks(var x = 1; typeof x == "string");': {
        'errors.0.message': 'Unexpected token var'
      }
    },
    'Object type property variance': {
      'type X = {p:T}': {
        'errors.length': 0,
        'body.0.right.properties.0.variance': null
      },
      'type X = {+p:T}': {
        'errors.length': 0,
        'body.0.right.properties.0.variance': 'plus'
      },
      'type X = {-p:T}': {
        'errors.length': 0,
        'body.0.right.properties.0.variance': 'minus'
      },
      'type X = {m():T}': {
        'errors.length': 0,
        'body.0.right.properties.0.variance': null
      },
      'type X = {+m():T}': {
        'errors.0': {
          'loc.start.column': 10,
          'loc.end.column': 11,
          'message': 'Unexpected variance sigil'
        }
      },
      'type X = {-m():T}': {
        'errors.0': {
          'loc.start.column': 10,
          'loc.end.column': 11,
          'message': 'Unexpected variance sigil'
        }
      },
      'type X = {[k:K]:V}': {
        'errors.length': 0,
        'body.0.right.indexers.0.variance': null
      },
      'type X = {+[k:K]:V}': {
        'errors.length': 0,
        'body.0.right.indexers.0.variance': 'plus'
      },
      'type X = {-[k:K]:V}': {
        'errors.length': 0,
        'body.0.right.indexers.0.variance': 'minus'
      },
      'type X = {():T}': {
        'errors.length': 0,
      },
      'type X = {+():T}': {
        'errors.0': {
          'loc.start.column': 10,
          'loc.end.column': 11,
          'message': 'Unexpected variance sigil'
        }
      },
      'type X = {-():T}': {
        'errors.0': {
          'loc.start.column': 10,
          'loc.end.column': 11,
          'message': 'Unexpected variance sigil'
        }
      }
    },
    'Class property variance': {
      'class C {p:T}': {
        'errors.length': 0,
        'body.0.body.body.0.variance': null
      },
      'class C {+p:T}': {
        'errors.length': 0,
        'body.0.body.body.0.variance': 'plus'
      },
      'class C {-p:T}': {
        'errors.length': 0,
        'body.0.body.body.0.variance': 'minus'
      },
      'class C {+m(){}}': {
        'errors.0': {
          'loc.start.column': 9,
          'loc.end.column': 10,
          'message': 'Unexpected variance sigil'
        }
      },
      'class C {-m() {}}': {
        'errors.0': {
          'loc.start.column': 9,
          'loc.end.column': 10,
          'message': 'Unexpected variance sigil'
        }
      },
      'class C {async +m() {}}': {
        'errors.0': {
          'loc.start.column': 15,
          'loc.end.column': 16,
          'message': 'Unexpected variance sigil'
        },
        'body.0.body.body.0.value.async': true
      },
      'class C {async -m():T {}}': {
        'errors.0': {
          'loc.start.column': 15,
          'loc.end.column': 16,
          'message': 'Unexpected variance sigil'
        },
        'body.0.body.body.0.value.async': true
      },
      'class C {*+m() {}}': {
        'errors.0': {
          'loc.start.column': 10,
          'loc.end.column': 11,
          'message': 'Unexpected variance sigil'
        },
        'body.0.body.body.0.value.generator': true
      },
      'class C {*-m():T {}}': {
        'errors.0': {
          'loc.start.column': 10,
          'loc.end.column': 11,
          'message': 'Unexpected variance sigil'
        },
        'body.0.body.body.0.value.generator': true
      },
      'class C {+*m() {}}': {
        'errors.0': {
          'loc.start.column': 9,
          'loc.end.column': 10,
          'message': 'Unexpected variance sigil'
        },
        'body.0.body.body.0.value.generator': true
      },
      'class C {-*m():T {}}': {
        'errors.0': {
          'loc.start.column': 9,
          'loc.end.column': 10,
          'message': 'Unexpected variance sigil'
        },
        'body.0.body.body.0.value.generator': true
      }
    },
    'Function Types with Anonymous Parameters': {
      'type A = (string) => void': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params.0': {
            'name': null,
            'typeAnnotation.type': 'StringTypeAnnotation',
          },
        }
      },
      'type A = (string,) => void': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params.0': {
            'name': null,
            'typeAnnotation.type': 'StringTypeAnnotation',
          },
        }
      },
      'type A = (Array<string>) => void': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params.0': {
            'name': null,
            'typeAnnotation.type': 'GenericTypeAnnotation',
            'typeAnnotation.typeParameters.params.0.type': 'StringTypeAnnotation',
          },
        }
      },
      'type A = (Array<string>,) => void': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params.0': {
            'name': null,
            'typeAnnotation.type': 'GenericTypeAnnotation',
            'typeAnnotation.typeParameters.params.0.type': 'StringTypeAnnotation',
          },
        }
      },
      'type A = (x: string, number) => void': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params.0': {
            'name.name': 'x',
            'typeAnnotation.type': 'StringTypeAnnotation',
          },
          'params.1': {
            'name': null,
            'typeAnnotation.type': 'NumberTypeAnnotation',
          },
        }
      },
      'type A = (...Array<string>) => void': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'rest': {
            'name': null,
            'typeAnnotation.type': 'GenericTypeAnnotation',
            'typeAnnotation.typeParameters.params.0.type': 'StringTypeAnnotation',
          },
        }
      },
      'type A = (Array<string>, ...Array<string>) => void': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params.0': {
            'name': null,
            'typeAnnotation.type': 'GenericTypeAnnotation',
            'typeAnnotation.typeParameters.params.0.type': 'StringTypeAnnotation',
          },
          'rest': {
            'name': null,
            'typeAnnotation.type': 'GenericTypeAnnotation',
            'typeAnnotation.typeParameters.params.0.type': 'StringTypeAnnotation',
          },
        }
      },
      // Non-anonymous function types are allowed as arrow function return types
      'var f = (x): (x: number) => 123 => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation.type': 'FunctionTypeAnnotation',
          'body.value': 123,
        }
      },
      // Anonymous function types are disallowed as arrow function return types
      // So the `=>` clearly belongs to the arrow function
      'var f = (): (number) => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation.type': 'NumberTypeAnnotation',
          'body.value': 123,
        }
      },
      'var f = (): string | (number) => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation': {
            'type': 'UnionTypeAnnotation',
            'types.1.type': 'NumberTypeAnnotation',
          },
          'body.value': 123,
        }
      },
      // You can write anonymous function types as arrow function return types
      // if you wrap them in parens
      'var f = (x): ((number) => 123) => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation.type': 'FunctionTypeAnnotation',
          'body.value': 123,
        }
      },
    },
    'Invalid Function Types with Anonymous Parameters': {
      // Anonymous function types are disallowed as arrow function return types
      'var f = (x): (number) => 123 => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation.type': 'NumberTypeAnnotation',
        },
        'errors.0': {
          'loc.start.column': 29,
          'loc.end.column': 31,
          'message': 'Unexpected token =>'
        },
      },
      'var f = (x): string | (number) => 123 => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation.type': 'UnionTypeAnnotation',
        },
        'errors.0': {
          'loc.start.column': 38,
          'loc.end.column': 40,
          'message': 'Unexpected token =>'
        },
      },
      'var f = (x): ?(number) => 123 => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation.type': 'NullableTypeAnnotation',
        },
        'errors.0': {
          'loc.start.column': 30,
          'loc.end.column': 32,
          'message': 'Unexpected token =>'
        },
      },
    },
    'Function Types with Anonymous Parameters and no Parens': {
      'type A = string => void': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params.0': {
            'name': null,
            'typeAnnotation.type': 'StringTypeAnnotation',
          },
        }
      },
      'type A = Array<string> => void': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params.0': {
            'name': null,
            'typeAnnotation.type': 'GenericTypeAnnotation',
            'typeAnnotation.typeParameters.params.0.type': 'StringTypeAnnotation',
          },
        }
      },
      // Anonymous function types are disallowed as arrow function return types
      // So the `=>` clearly belongs to the arrow function
      'var f = (): number => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation.type': 'NumberTypeAnnotation',
          'body.value': 123,
        }
      },
      'var f = (): string | number => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation': {
            'type': 'UnionTypeAnnotation',
            'types.1.type': 'NumberTypeAnnotation',
          },
          'body.value': 123,
        }
      },
      // You can write anonymous function types as arrow function return types
      // if you wrap them in parens
      'var f = (x): (number => 123) => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation.type': 'FunctionTypeAnnotation',
          'body.value': 123,
        }
      },
      // string | (number => boolean)
      'type A = string | number => boolean;': {
        'body.0.right': {
          'type': 'UnionTypeAnnotation',
          'types.1': {
            'type': 'FunctionTypeAnnotation',
            'params': [
              {
                'name': null,
                'typeAnnotation.type': 'NumberTypeAnnotation',
              }
            ]
          },
        },
      },
      // string & (number => boolean)
      'type A = string & number => boolean;': {
        'body.0.right': {
          'type': 'IntersectionTypeAnnotation',
          'types.1': {
            'type': 'FunctionTypeAnnotation',
            'params': [
              {
                'name': null,
                'typeAnnotation.type': 'NumberTypeAnnotation',
              }
            ]
          },
        },
      },
      // (?number) => boolean
      'type A = ?number => boolean;': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params': [
            {
              'name': null,
              'typeAnnotation.type': 'NullableTypeAnnotation',
            }
          ]
        },
      },
      // (number[]) => boolean
      'type A = number[] => boolean;': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params': [
            {
              'name': null,
              'typeAnnotation.type': 'ArrayTypeAnnotation',
            }
          ]
        },
      },
      'type A = (string => boolean) => number': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params': [
            {
              'name': null,
              'typeAnnotation.type': 'FunctionTypeAnnotation',
            }
          ]
        },
      },
      // string => (boolean | number)
      'type A = string => boolean | number;': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params': [
            {
              'name': null,
              'typeAnnotation.type': 'StringTypeAnnotation',
            }
          ]
        }
      },
      // Becomes string => (boolean => number)
      'type A = string => boolean => number;': {
        'body.0.right': {
          'type': 'FunctionTypeAnnotation',
          'params': [
            {
              'name': null,
              'typeAnnotation.type': 'StringTypeAnnotation',
            }
          ],
          'returnType': {
            'type': 'FunctionTypeAnnotation',
            'params': [
              {
                'name': null,
                'typeAnnotation.type': 'BooleanTypeAnnotation',
              }
            ],
            'returnType.type': 'NumberTypeAnnotation',
          }
        }
      },
    },
    'Invalid Function Types with Anonymous Parameters and no Parens': {
      // Anonymous function types are disallowed as arrow function return types
      'var f = (x): number => 123 => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation.type': 'NumberTypeAnnotation',
        },
        'errors.0': {
          'loc.start.column': 27,
          'loc.end.column': 29,
          'message': 'Unexpected token =>'
        },
      },
      'var f = (x): string | number => 123 => 123;': {
        'body.0.declarations.0.init': {
          'returnType.typeAnnotation.type': 'UnionTypeAnnotation',
        },
        'errors.0': {
          'loc.start.column': 36,
          'loc.end.column': 38,
          'message': 'Unexpected token =>'
        },
      },
    },
    'Optional indexer name': {
      'type A = { [string]: number };': {
        'body.0.right.indexers': [
          {
            'id': null,
            'key.type': 'StringTypeAnnotation',
            'value.type': 'NumberTypeAnnotation',
          }
        ]
      },
      'type A = { [string | boolean]: number };': {
        'body.0.right.indexers': [
          {
            'id': null,
            'key.type': 'UnionTypeAnnotation',
            'value.type': 'NumberTypeAnnotation',
          }
        ]
      },
    },
    'For await loops': {
      'for await (let x of e) {}': {
        'errors.0': {
          'loc.start.column': 4,
          'loc.end.column': 9,
          'message': 'Unexpected token await'
        }
      },
      'async () => { for await (let x of e) {} }': {
        'errors': [],
        'body.0.expression.body.body.0': {
          'type': 'ForAwaitStatement',
          'left.type': 'VariableDeclaration',
          'right.type': 'Identifier',
          'body.type': 'BlockStatement',
        }
      },
      'async () => { for await (let x in e) {} }': {
        'errors.0': {
          'loc.start.column': 31,
          'loc.end.column': 33,
          'message': 'Unexpected token in'
        }
      },
      'async () => { for await (let i = 0; i < e; i++) {} }': {
        'errors.0': {
          'loc.start.column': 25,
          'loc.end.column': 34,
          'message': 'Invalid left-hand side in for-of'
        }
      }
    },
    'Object type spread': {
      'type T = {...O}': {
        'errors': [],
        'body.0.right.properties': [{
          'type': 'ObjectTypeSpreadProperty',
          'argument': {
            'type': 'GenericTypeAnnotation',
            'id.name': 'O',
          },
        }],
      },
      'type T = {...O,}': {
        'errors.length': 0,
        'body.0.right.properties.length': 1,
      },
      'type T = {p:T, ...O}': {
        'errors.length': 0,
        'body.0.right.properties': [{
          'type': 'ObjectTypeProperty',
          'key.name': 'p',
        }, {
          'type': 'ObjectTypeSpreadProperty',
          'argument.id.name': 'O',
        }],
      },
      'type T = {...O, p:T}': {
        'errors.length': 0,
        'body.0.right.properties': [{
          'type': 'ObjectTypeSpreadProperty',
          'argument.id.name': 'O',
        }, {
          'type': 'ObjectTypeProperty',
          'key.name': 'p',
        }],
      },
      'type T = {...O1, ...O2}': {
        'errors.length': 0,
        'body.0.right.properties': [{
          'type': 'ObjectTypeSpreadProperty',
          'argument.id.name': 'O1',
        }, {
          'type': 'ObjectTypeSpreadProperty',
          'argument.id.name': 'O2',
        }],
      },
    },
    'Invalid instance spread': {
      'interface I {...O}': {
        'errors.0.message': 'Unexpected token ...',
      },
      'declare class C {...O}': {
        'errors.0.message': 'Unexpected token ...',
      },
    },
    'Array literal spreads': {
      '[1, ...rest]': {
        'body.0.expression.elements': [
          { 'value': 1 },
          { 'type': 'SpreadElement' },
        ]
      },
      '[1, ...rest,]': {
        'body.0.expression.elements': [
          { 'value': 1 },
          { 'type': 'SpreadElement' },
        ]
      },
      '[...rest, 1]': {
        'body.0.expression.elements': [
          { 'type': 'SpreadElement' },
          { 'value': 1 }
        ]
      },
      '[...rest, ,1]': {
        'body.0.expression.elements': [
          { 'type': 'SpreadElement' },
          null,
          { 'value': 1 }
        ]
      },
    },
    'import type shorthand': {
      'import {type} from "foo";': {
        'body.0': {
          'type': 'ImportDeclaration',
          'importKind': 'value',
          'specifiers': [{
            'type': 'ImportSpecifier',
            'imported.name': 'type',
            'local.name': 'type',
            'importKind': null,
          }]
        }
      },
      'import {type t} from "foo";': {
        'body.0': {
          'type': 'ImportDeclaration',
          'importKind': 'value',
          'specifiers': [{
            'type': 'ImportSpecifier',
            'imported.name': 't',
            'local.name': 't',
            'importKind': 'type',
          }]
        }
      },
      'import {type as} from "foo";': {
        'body.0': {
          'type': 'ImportDeclaration',
          'importKind': 'value',
          'specifiers': [{
            'type': 'ImportSpecifier',
            'imported.name': 'as',
            'local.name': 'as',
            'importKind': 'type',
          }]
        }
      },
      'import {type t as u} from "foo";': {
        'body.0': {
          'type': 'ImportDeclaration',
          'importKind': 'value',
          'specifiers': [{
            'type': 'ImportSpecifier',
            'imported.name': 't',
            'local.name': 'u',
            'importKind': 'type',
          }]
        }
      },

      'import {typeof t} from "foo";': {
        'body.0': {
          'type': 'ImportDeclaration',
          'importKind': 'value',
          'specifiers': [{
            'type': 'ImportSpecifier',
            'imported.name': 't',
            'local.name': 't',
            'importKind': 'typeof',
          }]
        }
      },
      'import {typeof as} from "foo";': {
        'body.0': {
          'type': 'ImportDeclaration',
          'importKind': 'value',
          'specifiers': [{
            'type': 'ImportSpecifier',
            'imported.name': 'as',
            'local.name': 'as',
            'importKind': 'typeof',
          }]
        }
      },
      'import {typeof t as u} from "foo";': {
        'body.0': {
          'type': 'ImportDeclaration',
          'importKind': 'value',
          'specifiers': [{
            'type': 'ImportSpecifier',
            'imported.name': 't',
            'local.name': 'u',
            'importKind': 'typeof',
          }]
        }
      },

      'import {type t as} from "foo";': {
        'errors.0.message': 'Unexpected token }'
      },
      'import {typeof} from "foo";': {
        'errors.0.message': 'Unexpected token typeof'
      },
      'import type {type t} from "foo";': {
        'errors.0.message': 'The `type` and `typeof` keywords on named imports can only be used on regular `import` statements. It cannot be used with `import type` or `import typeof` statements'
      },
      'import typeof {typeof t} from "foo";': {
        'errors.0.message': 'The `type` and `typeof` keywords on named imports can only be used on regular `import` statements. It cannot be used with `import type` or `import typeof` statements'
      },
    },
    'import statements': {
      'import {x,} from "MyModule";': {
        'body.0': {
          'type': 'ImportDeclaration',
          'importKind': 'value',
          'specifiers': [{
            'type': 'ImportSpecifier',
            'imported.name': 'x',
            'local.name': 'x',
          }]
        }
      },
      'import defexp, {x,} from "MyModule";': {
        'body.0': {
          'type': 'ImportDeclaration',
          'importKind': 'value',
          'specifiers': [
            {
              'type': 'ImportDefaultSpecifier',
              'local.name': 'defexp',
            },
            {
              'type': 'ImportSpecifier',
              'imported.name': 'x',
              'local.name': 'x',
            }
          ]
        }
      },
      'import {a nopeNeedsAPrecedingComma} from "MyModule";': {
        'errors.0.message': 'Missing comma between import specifiers'
      },
    },
  }
};
