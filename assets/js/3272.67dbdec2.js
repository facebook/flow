"use strict";(self.webpackChunknew_website=self.webpackChunknew_website||[]).push([[3272],{28453:(e,n,s)=>{s.d(n,{R:()=>r,x:()=>i});var t=s(96540);const a={},o=t.createContext(a);function r(e){const n=t.useContext(o);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function i(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:r(e.components),t.createElement(o.Provider,{value:n},e.children)}},33272:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>c,contentTitle:()=>i,default:()=>p,frontMatter:()=>r,metadata:()=>t,toc:()=>l});const t=JSON.parse('{"id":"types/casting","title":"Type Casting Expressions","description":"Sometimes it is useful to assert a type without using something like a function","source":"@site/docs/types/casting.md","sourceDirName":"types","slug":"/types/casting","permalink":"/en/docs/types/casting","draft":false,"unlisted":false,"editUrl":"https://github.com/facebook/flow/edit/main/website/docs/types/casting.md","tags":[],"version":"current","frontMatter":{"title":"Type Casting Expressions","slug":"/types/casting"},"sidebar":"docsSidebar","previous":{"title":"Typeof Types","permalink":"/en/docs/types/typeof"},"next":{"title":"Const Expressions","permalink":"/en/docs/types/const-expression"}}');var a=s(74848),o=s(28453);const r={title:"Type Casting Expressions",slug:"/types/casting"},i=void 0,c={},l=[{value:"Type Cast Expression Syntax",id:"toc-type-cast-expression-syntax",level:2},{value:"Type Assertions",id:"toc-type-assertions",level:3},{value:"Type Casting",id:"toc-type-casting",level:3},{value:"Adoption of <code>as</code> syntax",id:"adoption-of-as-syntax",level:3},{value:"Using type cast expressions",id:"toc-using-type-cast-expressions",level:2},{value:"Type Casting through <code>any</code>",id:"toc-type-casting-through-any",level:3},{value:"Legacy casting syntax",id:"legacy-casting-syntax",level:2}];function d(e){const n={a:"a",blockquote:"blockquote",code:"code",h2:"h2",h3:"h3",li:"li",p:"p",pre:"pre",strong:"strong",ul:"ul",...(0,o.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.p,{children:"Sometimes it is useful to assert a type without using something like a function\nor a variable to do so. For this Flow supports an inline type cast expression\nsyntax which can be used in a number of different ways."}),"\n",(0,a.jsx)(n.h2,{id:"toc-type-cast-expression-syntax",children:"Type Cast Expression Syntax"}),"\n",(0,a.jsxs)(n.p,{children:["In order to create a type cast expression, use the keyword ",(0,a.jsx)(n.code,{children:"as"})," to cast the value to a type:"]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-js",children:"value as Type\n"})}),"\n",(0,a.jsx)(n.p,{children:'This can also be referred to as an "as expression".'}),"\n",(0,a.jsxs)(n.blockquote,{children:["\n",(0,a.jsxs)(n.p,{children:["Before Flow version 0.229, the ",(0,a.jsx)(n.a,{href:"#legacy-casting-syntax",children:"legacy syntax"})," ",(0,a.jsx)(n.code,{children:"(value: Type)"})," was used."]}),"\n"]}),"\n",(0,a.jsx)(n.p,{children:"Type cast expressions can appear anywhere an expression can appear:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-js",children:"let val = value as Type;\nlet obj = {prop: value as Type};\nlet arr = [value as Type, value as Type] as Array<Type>;\n"})}),"\n",(0,a.jsx)(n.p,{children:"The value itself can also be an expression:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-flow",metastring:"[]",children:"2 + 2 as number;\n"})}),"\n",(0,a.jsxs)(n.p,{children:["Note that the ",(0,a.jsx)(n.code,{children:"as"})," operator has the same ",(0,a.jsx)(n.a,{href:"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence#table",children:"precedence"})," as ",(0,a.jsx)(n.code,{children:"in"})," and ",(0,a.jsx)(n.code,{children:"instanceof"}),".\nBecause of this, parentheses around the expression might be required:"]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-flow",metastring:'[{"startLine":1,"startColumn":7,"endLine":1,"endColumn":7,"description":"Cannot cast `1` to boolean because number [1] is incompatible with boolean [2]. [incompatible-cast]"}]',children:"1 === 1 as boolean; // Error!\n// Above same as `1 === (1 as boolean)\n\n(1 === 1) as boolean; // Works!\n"})}),"\n",(0,a.jsx)(n.p,{children:"Additionally, when in the context of an expression statement, expressions which could ambiguously parse as statements need parens:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-flow",metastring:"[]",children:"({a: 1}) as {a: number}; // Needs parens to disambiguate from block statement\nconst x = {a: 1} as {a: number}; // No parens needed, as not in expression statement context\n"})}),"\n",(0,a.jsx)(n.p,{children:"When you strip the types all that is left is the value:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-js",children:"value as Type;\n"})}),"\n",(0,a.jsx)(n.p,{children:"Is transformed into:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-js",children:"value;\n"})}),"\n",(0,a.jsx)(n.h3,{id:"toc-type-assertions",children:"Type Assertions"}),"\n",(0,a.jsx)(n.p,{children:"Using type cast expressions you can assert that values are certain types."}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-flow",metastring:'[{"startLine":5,"startColumn":1,"endLine":5,"endColumn":5,"description":"Cannot cast `value` to string because number [1] is incompatible with string [2]. [incompatible-cast]"}]',children:"let value = 42;\n\nvalue as 42;     // Works!\nvalue as number; // Works!\nvalue as string; // Error!\n"})}),"\n",(0,a.jsx)(n.p,{children:"Asserting types in this way works the same as types do anywhere else."}),"\n",(0,a.jsx)(n.h3,{id:"toc-type-casting",children:"Type Casting"}),"\n",(0,a.jsx)(n.p,{children:"When you write a type cast expression, the result of that expression is the\nvalue with the provided type. If you hold onto the resulting value, it will\nhave the new type."}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-flow",metastring:'[{"startLine":8,"startColumn":1,"endLine":8,"endColumn":8,"description":"Cannot cast `newValue` to number literal `42` because number [1] is incompatible with number literal `42` [2]. [incompatible-cast]"}]',children:"let value = 42;\n\nvalue as 42;     // Works!\nvalue as number; // Works!\n\nlet newValue = value as number;\n\nnewValue as 42;     // Error!\nnewValue as number; // Works!\n"})}),"\n",(0,a.jsx)(n.p,{children:"Unsafe downcasts are not allowed:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-flow",metastring:'[{"startLine":2,"startColumn":18,"endLine":2,"endColumn":23,"description":"Cannot cast `fooObj` to object type because property `bar` is missing in object literal [1] but exists in object type [2]. [prop-missing]"}]',children:"const fooObj = {foo: 1};\nconst otherObj = fooObj as {foo: number, bar: string};  // ERROR\n"})}),"\n",(0,a.jsxs)(n.h3,{id:"adoption-of-as-syntax",children:["Adoption of ",(0,a.jsx)(n.code,{children:"as"})," syntax"]}),"\n",(0,a.jsxs)(n.p,{children:["To use the ",(0,a.jsx)(n.code,{children:"as"})," keyword for type casts, you need to upgrade your infrastructure so that it supports the syntax:"]}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsx)(n.li,{children:"Flow and Flow Parser: 0.229+"}),"\n",(0,a.jsx)(n.li,{children:"Prettier: 3.1+"}),"\n",(0,a.jsxs)(n.li,{children:["Babel: use the ",(0,a.jsx)(n.a,{href:"https://www.npmjs.com/package/babel-plugin-syntax-hermes-parser",children:"babel-plugin-syntax-hermes-parser"})," plugin version 0.19+, see our ",(0,a.jsx)(n.a,{href:"../../tools/babel",children:"Babel guide"})," for more details."]}),"\n",(0,a.jsxs)(n.li,{children:["ESLint: use ",(0,a.jsx)(n.a,{href:"https://www.npmjs.com/package/hermes-eslint",children:"hermes-eslint"})," plugin version 0.19+, see our ",(0,a.jsx)(n.a,{href:"../../tools/eslint",children:"ESLint guide"})," for more details."]}),"\n"]}),"\n",(0,a.jsxs)(n.p,{children:["For more details on how to migrate to the new casting syntax (",(0,a.jsx)(n.code,{children:"as"}),") check out our ",(0,a.jsx)(n.a,{href:"https://medium.com/flow-type/new-type-casting-syntax-for-flow-as-3ef41567ff3e",children:"blog post"}),"."]}),"\n",(0,a.jsx)(n.h2,{id:"toc-using-type-cast-expressions",children:"Using type cast expressions"}),"\n",(0,a.jsxs)(n.blockquote,{children:["\n",(0,a.jsxs)(n.p,{children:[(0,a.jsx)(n.strong,{children:"Note:"})," We're going to go through a stripped down example for\ndemonstrating how to make use of type cast expressions. This example is not\nsolved well in practice."]}),"\n"]}),"\n",(0,a.jsxs)(n.h3,{id:"toc-type-casting-through-any",children:["Type Casting through ",(0,a.jsx)(n.code,{children:"any"})]}),"\n",(0,a.jsx)(n.p,{children:"Because type casts work the same as all other type annotations, you can only\ncast values to less specific types. You cannot change the type or make it\nsomething more specific."}),"\n",(0,a.jsxs)(n.p,{children:["But you can use ",(0,a.jsx)(n.a,{href:"../any",children:"any"})," to cast to whatever type you want."]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-flow",metastring:'[{"startLine":4,"startColumn":1,"endLine":4,"endColumn":5,"description":"Cannot cast `value` to string because number [1] is incompatible with string [2]. [incompatible-cast]"},{"startLine":8,"startColumn":1,"endLine":8,"endColumn":8,"description":"Cannot cast `newValue` to number because string [1] is incompatible with number [2]. [incompatible-cast]"}]',children:"let value = 42;\n\nvalue as number; // Works!\nvalue as string; // Error!\n\nlet newValue = value as any as string;\n\nnewValue as number; // Error!\nnewValue as string; // Works!\n"})}),"\n",(0,a.jsxs)(n.p,{children:["By casting the value to ",(0,a.jsx)(n.code,{children:"any"}),", you can then cast to whatever you want."]}),"\n",(0,a.jsx)(n.p,{children:"This is unsafe and not recommended. But it's sometimes useful when you are\ndoing something with a value which is very difficult or impossible to type and\nwant to make sure that the result has the desired type."}),"\n",(0,a.jsx)(n.p,{children:"For example, the following function for cloning an object."}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-flow",metastring:"[]",children:"function cloneObject(obj: any) {\n  const clone: {[string]: mixed} = {};\n\n  Object.keys(obj).forEach(key => {\n    clone[key] = obj[key];\n  });\n\n  return clone;\n}\n"})}),"\n",(0,a.jsx)(n.p,{children:"It would be hard to create a type for this because we're creating a new object\nbased on another object."}),"\n",(0,a.jsx)(n.p,{children:"If we cast through any, we can return a type which is more useful."}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-flow",metastring:"[]",children:"function cloneObject<T: {+[key: string]: mixed }>(obj: T): T {\n  const clone: {[string]: mixed} = {};\n\n  Object.keys(obj).forEach(key => {\n    clone[key] = obj[key];\n  });\n\n  return clone as any as T;\n}\n\nconst clone = cloneObject({\n  foo: 1,\n  bar: true,\n  baz: 'three'\n});\n\nclone.foo as 1;       // Works!\nclone.bar as true;    // Works!\nclone.baz as 'three'; // Works!\n"})}),"\n",(0,a.jsx)(n.h2,{id:"legacy-casting-syntax",children:"Legacy casting syntax"}),"\n",(0,a.jsxs)(n.p,{children:["Before version 0.229, to create a type cast expression around a ",(0,a.jsx)(n.code,{children:"value"}),", you would\nadd a colon ",(0,a.jsx)(n.code,{children:":"})," with the ",(0,a.jsx)(n.code,{children:"Type"})," and wrap the expression with parentheses ",(0,a.jsx)(n.code,{children:"("})," ",(0,a.jsx)(n.code,{children:")"}),"."]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-js",children:"(value: Type)\n"})}),"\n",(0,a.jsxs)(n.blockquote,{children:["\n",(0,a.jsxs)(n.p,{children:[(0,a.jsx)(n.strong,{children:"Note:"})," The parentheses are necessary to avoid ambiguity with other syntax."]}),"\n"]})]})}function p(e={}){const{wrapper:n}={...(0,o.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(d,{...e})}):d(e)}}}]);