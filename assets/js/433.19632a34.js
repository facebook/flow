"use strict";(self.webpackChunknew_website=self.webpackChunknew_website||[]).push([[433],{28453:(n,e,t)=>{t.d(e,{R:()=>s,x:()=>o});var r=t(96540);const i={},a=r.createContext(i);function s(n){const e=r.useContext(a);return r.useMemo((function(){return"function"==typeof n?n(e):{...e,...n}}),[e,n])}function o(n){let e;return e=n.disableParentContext?"function"==typeof n.components?n.components(i):n.components||i:s(n.components),r.createElement(a.Provider,{value:e},n.children)}},60433:(n,e,t)=>{t.r(e),t.d(e,{assets:()=>c,contentTitle:()=>o,default:()=>u,frontMatter:()=>s,metadata:()=>r,toc:()=>l});const r=JSON.parse('{"id":"types/functions","title":"Functions","description":"Functions have two places where types are applied: parameters (input) and the return value (output).","source":"@site/docs/types/functions.md","sourceDirName":"types","slug":"/types/functions","permalink":"/en/docs/types/functions","draft":false,"unlisted":false,"editUrl":"https://github.com/facebook/flow/edit/main/website/docs/types/functions.md","tags":[],"version":"current","frontMatter":{"title":"Functions","slug":"/types/functions"},"sidebar":"docsSidebar","previous":{"title":"Maybe Types","permalink":"/en/docs/types/maybe"},"next":{"title":"Objects","permalink":"/en/docs/types/objects"}}');var i=t(74848),a=t(28453);const s={title:"Functions",slug:"/types/functions"},o=void 0,c={},l=[{value:"Syntax of functions",id:"syntax-of-functions",level:2},{value:"Function Declarations",id:"function-declarations",level:3},{value:"Arrow Functions",id:"arrow-functions",level:3},{value:"Function Types",id:"function-types",level:3},{value:"Type arguments",id:"type-arguments",level:3},{value:"Function Parameters",id:"function-parameters",level:2},{value:"Optional Parameters",id:"optional-parameters",level:3},{value:"Rest Parameters",id:"rest-parameters",level:3},{value:"<code>this</code> parameter",id:"this-parameter",level:3},{value:"Function Returns",id:"function-returns",level:2},{value:"Predicate Functions",id:"predicate-functions",level:3},{value:"Limitations of predicate functions",id:"limitations-of-predicate-functions",level:4},{value:"Callable Objects",id:"callable-objects",level:2},{value:"Overloaded functions",id:"overloaded-functions",level:2},{value:"Any function",id:"any-function",level:2}];function d(n){const e={a:"a",admonition:"admonition",blockquote:"blockquote",code:"code",em:"em",h2:"h2",h3:"h3",h4:"h4",p:"p",pre:"pre",...(0,a.R)(),...n.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(e.p,{children:"Functions have two places where types are applied: parameters (input) and the return value (output)."}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:'[{"startLine":6,"startColumn":8,"endLine":6,"endColumn":11,"description":"Cannot call `concat` with `true` bound to `a` because boolean [1] is incompatible with string [2]. [incompatible-call]"},{"startLine":6,"startColumn":14,"endLine":6,"endColumn":18,"description":"Cannot call `concat` with `false` bound to `b` because boolean [1] is incompatible with string [2]. [incompatible-call]"}]',children:'function concat(a: string, b: string): string {\n  return a + b;\n}\n\nconcat("foo", "bar"); // Works!\nconcat(true, false);  // Error!\n'})}),"\n",(0,i.jsx)(e.p,{children:"Using inference, return types are often optional:"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:'function concat(a: string, b: string) {\n  return a + b;\n}\n\nconst s: string = concat("foo", "bar"); // Works!\n'})}),"\n",(0,i.jsx)(e.p,{children:"If defined where we can get the type from the context of the expression, type annotations can be optional:"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"[1, 2, 3].map(x => x * x); // From the context, we know parameter `x` has type `number`\n"})}),"\n",(0,i.jsx)(e.h2,{id:"syntax-of-functions",children:"Syntax of functions"}),"\n",(0,i.jsx)(e.p,{children:"There are three forms of functions that each have their own slightly different syntax."}),"\n",(0,i.jsx)(e.h3,{id:"function-declarations",children:"Function Declarations"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function func(str: string, bool?: boolean, ...nums: Array<number>): void {\n  // ...\n}\n"})}),"\n",(0,i.jsx)(e.h3,{id:"arrow-functions",children:"Arrow Functions"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"let func = (str: string, bool?: boolean, ...nums: Array<number>): void => {\n  // ...\n};\n"})}),"\n",(0,i.jsx)(e.h3,{id:"function-types",children:"Function Types"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"type T = (str: string, bool?: boolean, ...nums: Array<number>) => void;\n"})}),"\n",(0,i.jsx)(e.p,{children:"You may also optionally leave out the parameter names."}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"type T = (string, boolean | void, Array<number>) => void;\n"})}),"\n",(0,i.jsx)(e.p,{children:"You might use these functions types for something like a callback."}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function func(callback: (error: Error | null, value: string | null) => void) {\n  // ...\n}\n"})}),"\n",(0,i.jsx)(e.h3,{id:"type-arguments",children:"Type arguments"}),"\n",(0,i.jsx)(e.p,{children:"Functions can have type arguments:"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function f<T>(x: T): Array<T> {\n  return [x];\n}\n\nconst g = <T>(x: T): Array<T> => [x];\n\ntype H = <T>(T) => Array<T>;\n"})}),"\n",(0,i.jsx)(e.h2,{id:"function-parameters",children:"Function Parameters"}),"\n",(0,i.jsxs)(e.p,{children:["Function parameters can have types by adding a colon ",(0,i.jsx)(e.code,{children:":"})," followed by the type\nafter the name of the parameter."]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function func(param1: string, param2: boolean) {\n  // ...\n}\n"})}),"\n",(0,i.jsx)(e.h3,{id:"optional-parameters",children:"Optional Parameters"}),"\n",(0,i.jsxs)(e.p,{children:["You can also have optional parameters by adding a question mark ",(0,i.jsx)(e.code,{children:"?"})," after the\nname of the parameter and before the colon ",(0,i.jsx)(e.code,{children:":"}),"."]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function func(optionalValue?: string) {\n  // ...\n}\n"})}),"\n",(0,i.jsxs)(e.p,{children:["Optional parameters will accept missing, ",(0,i.jsx)(e.code,{children:"undefined"}),", or matching types. But\nthey will not accept ",(0,i.jsx)(e.code,{children:"null"}),"."]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:'[{"startLine":9,"startColumn":6,"endLine":9,"endColumn":9,"description":"Cannot call `func` with `null` bound to `optionalValue` because null [1] is incompatible with string [2]. [incompatible-call]"}]',children:'function func(optionalValue?: string) {\n  // ...\n}\n\nfunc();          // Works.\nfunc(undefined); // Works.\nfunc("string");  // Works.\n\nfunc(null);      // Error!\n'})}),"\n",(0,i.jsx)(e.h3,{id:"rest-parameters",children:"Rest Parameters"}),"\n",(0,i.jsxs)(e.p,{children:["JavaScript also supports having rest parameters or parameters that collect an\narray of arguments at the end of a list of parameters. These have an ellipsis\n",(0,i.jsx)(e.code,{children:"..."})," before them."]}),"\n",(0,i.jsxs)(e.p,{children:["You can also add type annotations for rest parameters using the same syntax but\nwith an ",(0,i.jsx)(e.code,{children:"Array"}),"."]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function func(...args: Array<number>) {\n  // ...\n}\n"})}),"\n",(0,i.jsx)(e.p,{children:"You can pass as many arguments as you want into a rest parameter."}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function func(...args: Array<number>) {\n  // ...\n}\n\nfunc();        // Works.\nfunc(1);       // Works.\nfunc(1, 2);    // Works.\nfunc(1, 2, 3); // Works.\n"})}),"\n",(0,i.jsxs)(e.blockquote,{children:["\n",(0,i.jsxs)(e.p,{children:["Note: If you add a type annotation to a rest parameter, it must always\nexplicitly be an ",(0,i.jsx)(e.code,{children:"Array"})," of ",(0,i.jsx)(e.code,{children:"$ReadOnlyArray"})," type."]}),"\n"]}),"\n",(0,i.jsxs)(e.h3,{id:"this-parameter",children:[(0,i.jsx)(e.code,{children:"this"})," parameter"]}),"\n",(0,i.jsxs)(e.p,{children:["Every function in JavaScript can be called with a special context named ",(0,i.jsx)(e.code,{children:"this"}),".\nYou can call a function with any context that you want. Flow allows you to annotate\nthe type for this context by adding a special parameter at the start of the function's parameter list:"]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:'[{"startLine":6,"startColumn":21,"endLine":6,"endColumn":39,"description":"Cannot assign `func.call(...)` to `str` because number [1] is incompatible with string [2]. [incompatible-type]"}]',children:"function func<T>(this: { x: T }) : T {\n  return this.x;\n}\n\nconst num: number = func.call({x : 42});\nconst str: string = func.call({x : 42}); // Error!\n"})}),"\n",(0,i.jsxs)(e.p,{children:["This parameter has no effect at runtime, and is erased along with types when Flow is transformed into JavaScript.\nWhen present, ",(0,i.jsx)(e.code,{children:"this"})," parameters must always appear at the very beginning of the function's parameter list, and must\nhave an annotation. Additionally, ",(0,i.jsx)(e.a,{href:"./#toc-arrow-functions",children:"arrow functions"})," may not have a ",(0,i.jsx)(e.code,{children:"this"})," parameter annotation, as\nthese functions bind their ",(0,i.jsx)(e.code,{children:"this"})," parameter at the definition site, rather than the call site."]}),"\n",(0,i.jsxs)(e.p,{children:["If an explicit ",(0,i.jsx)(e.code,{children:"this"})," parameter is not provided, Flow will attempt to infer one based on usage. If ",(0,i.jsx)(e.code,{children:"this"})," is not mentioned\nin the body of the function, Flow will infer ",(0,i.jsx)(e.code,{children:"mixed"})," for its ",(0,i.jsx)(e.code,{children:"this"})," parameter."]}),"\n",(0,i.jsx)(e.h2,{id:"function-returns",children:"Function Returns"}),"\n",(0,i.jsxs)(e.p,{children:["Function returns can also add a type using a colon ",(0,i.jsx)(e.code,{children:":"})," followed by the type\nafter the list of parameters."]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function func(): number {\n  return 1;\n}\n"})}),"\n",(0,i.jsx)(e.p,{children:"Return types ensure that every branch of your function returns the same type.\nThis prevents you from accidentally not returning a value under certain\nconditions."}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:'[{"startLine":1,"startColumn":18,"endLine":1,"endColumn":24,"description":"Cannot expect boolean as the return type of function because boolean [1] is incompatible with implicitly-returned undefined. [incompatible-return]"}]',children:"function func(): boolean {\n  if (Math.random() > 0.5) {\n    return true;\n  }\n}\n"})}),"\n",(0,i.jsxs)(e.p,{children:["Async functions implicitly return a promise, so the return type must always be a ",(0,i.jsx)(e.code,{children:"Promise"}),"."]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"async function func(): Promise<number> {\n  return 123;\n}\n"})}),"\n",(0,i.jsx)(e.h3,{id:"predicate-functions",children:"Predicate Functions"}),"\n",(0,i.jsx)(e.admonition,{type:"warning",children:(0,i.jsxs)(e.p,{children:["Predicate functions are deprecated and will be removed in a future version. Use ",(0,i.jsx)(e.a,{href:"../type-guards",children:"type guards"})," instead."]})}),"\n",(0,i.jsxs)(e.p,{children:["Sometimes you will want to move the condition from an ",(0,i.jsx)(e.code,{children:"if"})," statement into a function:"]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function concat(a: ?string, b: ?string): string {\n  if (a != null && b != null) {\n    return a + b;\n  }\n  return '';\n}\n"})}),"\n",(0,i.jsx)(e.p,{children:"However, Flow will error in the code below:"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:'[{"startLine":7,"startColumn":12,"endLine":7,"endColumn":16,"description":"Cannot use operator `+` with operands null or undefined [1] and null or undefined [2] [unsafe-addition]"},{"startLine":7,"startColumn":12,"endLine":7,"endColumn":16,"description":"Cannot use operator `+` with operands null or undefined [1] and string [2] [unsafe-addition]"},{"startLine":7,"startColumn":12,"endLine":7,"endColumn":16,"description":"Cannot use operator `+` with operands string [1] and null or undefined [2] [unsafe-addition]"}]',children:"function truthy(a: ?string, b: ?string): boolean {\n  return a != null && b != null;\n}\n\nfunction concat(a: ?string, b: ?string): string {\n  if (truthy(a, b)) {\n    return a + b; // Error!\n  }\n  return '';\n}\n"})}),"\n",(0,i.jsxs)(e.p,{children:["This is because the refinement information of ",(0,i.jsx)(e.code,{children:"a"})," and ",(0,i.jsx)(e.code,{children:"b"})," as ",(0,i.jsx)(e.code,{children:"string"})," instead of ",(0,i.jsx)(e.code,{children:"?string"})," is lost when returning from the ",(0,i.jsx)(e.code,{children:"truthy"})," function."]}),"\n",(0,i.jsxs)(e.p,{children:["You can fix this by making ",(0,i.jsx)(e.code,{children:"truthy"})," a ",(0,i.jsx)(e.em,{children:"predicate function"}),", by using the ",(0,i.jsx)(e.code,{children:"%checks"})," annotation like so:"]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:'[{"startLine":1,"startColumn":50,"endLine":1,"endColumn":56,"description":"Support for predicate functions is removed. `%checks` declaration is now ignored. [unsupported-syntax]"},{"startLine":7,"startColumn":12,"endLine":7,"endColumn":16,"description":"Cannot use operator `+` with operands null or undefined [1] and null or undefined [2] [unsafe-addition]"},{"startLine":7,"startColumn":12,"endLine":7,"endColumn":16,"description":"Cannot use operator `+` with operands null or undefined [1] and string [2] [unsafe-addition]"},{"startLine":7,"startColumn":12,"endLine":7,"endColumn":16,"description":"Cannot use operator `+` with operands string [1] and null or undefined [2] [unsafe-addition]"}]',children:"function truthy(a: ?string, b: ?string): boolean %checks {\n  return a != null && b != null;\n}\n\nfunction concat(a: ?string, b: ?string): string {\n  if (truthy(a, b)) {\n    return a + b;\n  }\n  return '';\n}\n"})}),"\n",(0,i.jsx)(e.h4,{id:"limitations-of-predicate-functions",children:"Limitations of predicate functions"}),"\n",(0,i.jsx)(e.p,{children:"The body of these predicate functions need to be expressions (i.e. local variable declarations are not supported).\nBut it's possible to call other predicate functions inside a predicate function.\nFor example:"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:'[{"startLine":1,"startColumn":28,"endLine":1,"endColumn":36,"description":"Support for predicate functions is removed. `%checks` declaration is now ignored. [unsupported-syntax]"},{"startLine":5,"startColumn":28,"endLine":5,"endColumn":36,"description":"Support for predicate functions is removed. `%checks` declaration is now ignored. [unsupported-syntax]"},{"startLine":9,"startColumn":36,"endLine":9,"endColumn":44,"description":"Support for predicate functions is removed. `%checks` declaration is now ignored. [unsupported-syntax]"},{"startLine":15,"startColumn":12,"endLine":15,"endColumn":16,"description":"Cannot use operator `+` with operands string [1] and array type [2] [unsafe-addition]"},{"startLine":15,"startColumn":12,"endLine":15,"endColumn":16,"description":"Cannot use operator `+` with operands number [1] and array type [2] [unsafe-addition]"},{"startLine":15,"startColumn":12,"endLine":15,"endColumn":16,"description":"Cannot use operator `+` with operands array type [1] and string [2] [unsafe-addition]"},{"startLine":15,"startColumn":12,"endLine":15,"endColumn":16,"description":"Cannot use operator `+` with operands array type [1] and number [2] [unsafe-addition]"},{"startLine":15,"startColumn":12,"endLine":15,"endColumn":16,"description":"Cannot use operator `+` with operands array type [1] and array type [1] [unsafe-addition]"},{"startLine":17,"startColumn":14,"endLine":17,"endColumn":19,"description":"Cannot get `x.length` because property `length` is missing in `Number` [1]. [prop-missing]"}]',children:'function isString(y: mixed): %checks {\n  return typeof y === "string";\n}\n\nfunction isNumber(y: mixed): %checks {\n  return typeof y === "number";\n}\n\nfunction isNumberOrString(y: mixed): %checks {\n  return isString(y) || isNumber(y);\n}\n\nfunction foo(x: string | number | Array<mixed>): string | number {\n  if (isNumberOrString(x)) {\n    return x + x;\n  } else {\n    return x.length; // no error, because Flow infers that x can only be an array\n  }\n}\n\nfoo(\'a\');\nfoo(5);\nfoo([]);\n'})}),"\n",(0,i.jsx)(e.p,{children:"Another limitation is on the range of predicates that can be encoded. The refinements\nthat are supported in a predicate function must refer directly to the value that\nis passed in as an argument to the respective call."}),"\n",(0,i.jsxs)(e.p,{children:["For example, consider the ",(0,i.jsx)(e.em,{children:"inlined"})," refinement"]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"declare const obj: {n?: number};\n\nif (obj.n != null) {\n  const n: number = obj.n;\n}\n"})}),"\n",(0,i.jsxs)(e.p,{children:["Here, Flow will let you refine ",(0,i.jsx)(e.code,{children:"obj.n"})," from ",(0,i.jsx)(e.code,{children:"?number"})," to ",(0,i.jsx)(e.code,{children:"number"}),". Note that the\nrefinement here is on the property ",(0,i.jsx)(e.code,{children:"n"})," of ",(0,i.jsx)(e.code,{children:"obj"}),", rather than ",(0,i.jsx)(e.code,{children:"obj"})," itself."]}),"\n",(0,i.jsxs)(e.p,{children:["If you tried to create a ",(0,i.jsx)(e.em,{children:"predicate"})," function to encode the same condition,\nthen the following refinement would fail"]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:'[{"startLine":1,"startColumn":35,"endLine":1,"endColumn":43,"description":"Support for predicate functions is removed. `%checks` declaration is now ignored. [unsupported-syntax]"},{"startLine":8,"startColumn":21,"endLine":8,"endColumn":25,"description":"Cannot assign `obj.n` to `n` because undefined [1] is incompatible with number [2]. [incompatible-type]"}]',children:"function bar(a: {n?: number, ...}): %checks {\n  return a.n != null;\n}\n\ndeclare const obj: {n?: number};\n\nif (bar(obj)) {\n  const n: number = obj.n; // Error\n}\n"})}),"\n",(0,i.jsxs)(e.p,{children:["This is because the only refinements supported through ",(0,i.jsx)(e.code,{children:"bar"})," would be on ",(0,i.jsx)(e.code,{children:"obj"})," itself."]}),"\n",(0,i.jsx)(e.h2,{id:"callable-objects",children:"Callable Objects"}),"\n",(0,i.jsx)(e.p,{children:"Callable objects can be typed, for example:"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:'type CallableObj = {\n  (number, number): number,\n  bar: string,\n  ...\n};\n\nfunction add(x: number, y: number) {\n  return x + y;\n}\n\nadd.bar = "hello world";\n\nadd as CallableObj;\n'})}),"\n",(0,i.jsxs)(e.p,{children:["In general, functions can have properties assigned to them if they are function declarations, or\nsimple variable declarations of the form ",(0,i.jsx)(e.code,{children:"const f = () => ..."}),". The properties must be assigned in\nthe format ",(0,i.jsx)(e.code,{children:"f.prop = <expr>;"}),", in the same statement list as the function definition (i.e. not conditionally)."]}),"\n",(0,i.jsx)(e.p,{children:"Note that the object representing the static properties assigned to the function is inexact."}),"\n",(0,i.jsx)(e.h2,{id:"overloaded-functions",children:"Overloaded functions"}),"\n",(0,i.jsxs)(e.p,{children:["You can use intersection types to define ",(0,i.jsx)(e.a,{href:"../intersections/#toc-intersection-of-function-types",children:"overloaded function types"}),":"]}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"declare const fn:\n  & ((x: 'string') => string)\n  & ((x: 'number') => number)\n\nconst s: string = fn('string');\nconst n: number = fn('number');\n"})}),"\n",(0,i.jsx)(e.h2,{id:"any-function",children:"Any function"}),"\n",(0,i.jsx)(e.p,{children:"If you want to specify you want to allow any function, and do not care what it is, you can use this pattern:"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function useCallback<T: (...$ReadOnlyArray<empty>) => mixed>(\n  callback: T,\n  inputs: ?$ReadOnlyArray<mixed>,\n): T {\n  return callback;\n}\nuseCallback((x: string) => true); // OK\nuseCallback((x: number) => [1]); // OK\n"})}),"\n",(0,i.jsx)(e.p,{children:"You could use type arguments to capture the arguments and return type, to do more complicated transformations:"}),"\n",(0,i.jsx)(e.pre,{children:(0,i.jsx)(e.code,{className:"language-flow",metastring:"[]",children:"function func<TArgs: $ReadOnlyArray<mixed>, TReturn>(\n  callback: (...TArgs) => TReturn,\n): (boolean, ...TArgs) => Array<TReturn> {\n  return (b, ...args): Array<TReturn> => {\n    if (b) {\n      return [callback(...args)];\n    } else {\n      return [];\n    }\n  };\n}\n\nconst f: (boolean, string, number) => Array<string> =\n  func((x: string, y: number) => x.slice(y)); // OK\n"})}),"\n",(0,i.jsxs)(e.p,{children:["The type ",(0,i.jsx)(e.code,{children:"Function"})," is just an alias for ",(0,i.jsx)(e.a,{href:"../any",children:(0,i.jsx)(e.code,{children:"any"})}),", and is unsafe.\nYou can ban its use in your code with the ",(0,i.jsx)(e.a,{href:"../../linting/rule-reference/#toc-unclear-type",children:"unclear-type lint"}),"."]})]})}function u(n={}){const{wrapper:e}={...(0,a.R)(),...n.components};return e?(0,i.jsx)(e,{...n,children:(0,i.jsx)(d,{...n})}):d(n)}}}]);