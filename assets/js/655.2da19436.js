"use strict";(self.webpackChunknew_website=self.webpackChunknew_website||[]).push([[655],{10655:(e,r,s)=>{s.r(r),s.d(r,{assets:()=>a,contentTitle:()=>c,default:()=>u,frontMatter:()=>l,metadata:()=>o,toc:()=>p});const o=JSON.parse('{"id":"errors/index","title":"Error Suppressions","description":"Learn how to suppress Flow\'s type errors.","source":"@site/docs/errors/index.md","sourceDirName":"errors","slug":"/errors","permalink":"/en/docs/errors","draft":false,"unlisted":false,"editUrl":"https://github.com/facebook/flow/edit/main/website/docs/errors/index.md","tags":[],"version":"current","frontMatter":{"title":"Error Suppressions","slug":"/errors","description":"Learn how to suppress Flow\'s type errors."},"sidebar":"docsSidebar","previous":{"title":"Creating Library Definitions","permalink":"/en/docs/libdefs/creation"},"next":{"title":"Linting Overview","permalink":"/en/docs/linting"}}');var n=s(74848),i=s(28453),t=s(86543);const l={title:"Error Suppressions",slug:"/errors",description:"Learn how to suppress Flow's type errors."},c=void 0,a={},p=[{value:"What is a Suppression?",id:"toc-what-is-a-suppression",level:3},{value:"Making Suppressions More Granular with Error Codes",id:"toc-making-suppressions-more-granular-with-error-codes",level:3},{value:"Unsuppressable Errors <SinceVersion></SinceVersion>",id:"toc-unsuppressable-errors",level:3}];function d(e){const r={a:"a",code:"code",h3:"h3",li:"li",p:"p",pre:"pre",ul:"ul",...(0,i.R)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(r.p,{children:"Flow reports many different kinds of errors for many common programming mistakes, but not every JavaScript pattern can be understood by Flow.\nIf you are confident your code is correct, and that Flow is\nerroring too conservatively, you can suppress the error so that\nFlow does not report it."}),"\n",(0,n.jsx)(r.h3,{id:"toc-what-is-a-suppression",children:"What is a Suppression?"}),"\n",(0,n.jsx)(r.p,{children:"A suppression is a special kind of comment that you can place on the line before a type\nerror. It tells Flow not to report that error when checking your code. Suppression\ncomments look like the following:"}),"\n",(0,n.jsx)(r.pre,{children:(0,n.jsx)(r.code,{children:"// <SUPPRESSOR>[<CODE>] extra text\n"})}),"\n",(0,n.jsx)(r.p,{children:"A suppressor can be one of the following:"}),"\n",(0,n.jsxs)(r.ul,{children:["\n",(0,n.jsxs)(r.li,{children:[(0,n.jsx)(r.code,{children:"$FlowFixMe"}),": for type errors that you intend to fix later"]}),"\n",(0,n.jsxs)(r.li,{children:[(0,n.jsx)(r.code,{children:"$FlowIssue"}),": for a type error that you suspect is an issue with Flow"]}),"\n",(0,n.jsxs)(r.li,{children:[(0,n.jsx)(r.code,{children:"$FlowExpectedError"}),": for a location where you expect Flow to produce a type error (for instance, when performing an invalid type cast)."]}),"\n",(0,n.jsxs)(r.li,{children:[(0,n.jsx)(r.code,{children:"$FlowIgnore"}),": for locations where you want Flow to ignore your code"]}),"\n"]}),"\n",(0,n.jsx)(r.p,{children:"Note that all of the suppressors behave identically; we simply recommend using them as described here for your own ease of reference."}),"\n",(0,n.jsxs)(r.p,{children:["The ",(0,n.jsx)(r.code,{children:"<CODE>"})," portion of a suppression is optional for most of ",(0,n.jsx)(r.a,{href:"#toc-making-suppressions-more-granular-with-error-codes",children:"error codes"}),". Some error codes cannot be suppressed without specific-error code (e.g. ",(0,n.jsx)(r.code,{children:"react-rule"})," related ones). When an error code is included, it specifies which error code the suppression affects."]}),"\n",(0,n.jsx)(r.p,{children:"Some examples of suppression comments:"}),"\n",(0,n.jsx)(r.pre,{children:(0,n.jsx)(r.code,{children:"// $FlowFixMe\n\n// $FlowIssue[incompatible-type]\n\n/* $FlowIgnore[prop-missing] some other text here */\n\n/* $FlowFixMe[incompatible-cast] this\n    is a multi-line\n    comment */\n\n{ /* $FlowIssue this is how you suppress errors inside JSX */ }\n"})}),"\n",(0,n.jsx)(r.p,{children:"In order to be a valid suppression comment, there are also some conditions that must be true:"}),"\n",(0,n.jsxs)(r.ul,{children:["\n",(0,n.jsxs)(r.li,{children:["No text can precede the suppressor, or come between the suppressor and the code. For example: ",(0,n.jsx)(r.code,{children:"// some text then $FlowFixMe"})," is not a valid suppression, nor is ",(0,n.jsx)(r.code,{children:"// $FlowIssue some text [incompatible-type]"})," or ",(0,n.jsx)(r.code,{children:" //$FlowFixMe [prop-missing]"})," (note the space here!)."]}),"\n",(0,n.jsx)(r.li,{children:"Suppressions must be on the line immediately before the error they suppress, otherwise they will not apply."}),"\n"]}),"\n",(0,n.jsx)(r.h3,{id:"toc-making-suppressions-more-granular-with-error-codes",children:"Making Suppressions More Granular with Error Codes"}),"\n",(0,n.jsx)(r.p,{children:"Suppressible Flow errors will also have an error code associated with them (after version 0.127). This code concisely describes the type of issue the error is reporting, and is different between different kinds of errors."}),"\n",(0,n.jsxs)(r.p,{children:["In order to prevent suppressions from suppressing different kinds of type errors on the same line (by default suppressions without codes suppress every error on the following line), you can add an error code to your suppression. For example: ",(0,n.jsx)(r.code,{children:"// $FlowFixMe[incompatible-cast]"})," would only suppress errors with the ",(0,n.jsx)(r.code,{children:"incompatible-cast"})," code. So:"]}),"\n",(0,n.jsx)(r.pre,{children:(0,n.jsx)(r.code,{className:"language-flow",metastring:"[]",children:"// $FlowFixMe[incompatible-cast]\n3 as string;\n"})}),"\n",(0,n.jsx)(r.p,{children:"would report no errors, but"}),"\n",(0,n.jsx)(r.pre,{children:(0,n.jsx)(r.code,{className:"language-flow",metastring:'[{"startLine":2,"startColumn":1,"endLine":2,"endColumn":1,"description":"Cannot cast `3` to string because number [1] is incompatible with string [2]. [incompatible-cast]"}]',children:"// $FlowFixMe[prop-missing]\n3 as string;\n"})}),"\n",(0,n.jsx)(r.p,{children:"would still report a type incompatibility."}),"\n",(0,n.jsx)(r.p,{children:"To suppress multiple error codes on the same line, you can stack suppression comments one after another, and they will all apply to the first non-comment line like so:"}),"\n",(0,n.jsx)(r.pre,{children:(0,n.jsx)(r.code,{className:"language-flow",metastring:"[]",children:"let y: number | {x : number}  = 1;\n\n// $FlowFixMe[incompatible-cast]\n// $FlowFixMe[prop-missing]\ny.x as string;\n"})}),"\n",(0,n.jsx)(r.p,{children:"This will suppress both of the two errors on this line."}),"\n",(0,n.jsxs)(r.h3,{id:"toc-unsuppressable-errors",children:["Unsuppressable Errors ",(0,n.jsx)(t.V,{version:"0.268"})]}),"\n",(0,n.jsxs)(r.p,{children:["Certain kinds of errors can be made unsuppressable. For example, to make ",(0,n.jsx)(r.code,{children:"react-rule-hook-naming-convention"})," and ",(0,n.jsx)(r.code,{children:"react-rule-hook-conditional"})," errors unsuppressable, you can add the following to the ",(0,n.jsx)(r.code,{children:"[options]"})," section in ",(0,n.jsx)(r.code,{children:"flowconfig"}),":"]}),"\n",(0,n.jsx)(r.pre,{children:(0,n.jsx)(r.code,{children:"unsuppressable_error_codes=react-rule-hook-naming-convention\nunsuppressable_error_codes=react-rule-hook-conditional\n"})})]})}function u(e={}){const{wrapper:r}={...(0,i.R)(),...e.components};return r?(0,n.jsx)(r,{...e,children:(0,n.jsx)(d,{...e})}):d(e)}},28453:(e,r,s)=>{s.d(r,{R:()=>t,x:()=>l});var o=s(96540);const n={},i=o.createContext(n);function t(e){const r=o.useContext(i);return o.useMemo((function(){return"function"==typeof e?e(r):{...r,...e}}),[r,e])}function l(e){let r;return r=e.disableParentContext?"function"==typeof e.components?e.components(n):e.components||n:t(e.components),o.createElement(i.Provider,{value:r},e.children)}},86543:(e,r,s)=>{s.d(r,{V:()=>n,v:()=>i});s(96540);var o=s(74848);function n(e){let{version:r}=e;return(0,o.jsxs)("span",{class:"version added",title:`Added in ${r}`,children:["\u2265",r]})}function i(e){let{version:r}=e;return(0,o.jsxs)("span",{class:"version removed",title:`Removed after ${r}`,children:["\u2264",r]})}}}]);