"use strict";(self.webpackChunknew_website=self.webpackChunknew_website||[]).push([[9492],{28453:(e,t,n)=>{n.d(t,{R:()=>l,x:()=>r});var o=n(96540);const i={},a=o.createContext(i);function l(e){const t=o.useContext(a);return o.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function r(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:l(e.components),o.createElement(a.Provider,{value:t},e.children)}},89492:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>r,default:()=>m,frontMatter:()=>l,metadata:()=>o,toc:()=>c});const o=JSON.parse('{"id":"react/multiplatform","title":"Multi-platform Support for React Native","description":"Flow\'s support for multiple platforms inside a single React Native codebase","source":"@site/docs/react/multiplatform.md","sourceDirName":"react","slug":"/react/multiplatform","permalink":"/en/docs/react/multiplatform","draft":false,"unlisted":false,"editUrl":"https://github.com/facebook/flow/edit/main/website/docs/react/multiplatform.md","tags":[],"version":"current","frontMatter":{"title":"Multi-platform Support for React Native","slug":"/react/multiplatform","description":"Flow\'s support for multiple platforms inside a single React Native codebase"}}');var i=n(74848),a=n(28453);const l={title:"Multi-platform Support for React Native",slug:"/react/multiplatform",description:"Flow's support for multiple platforms inside a single React Native codebase"},r=void 0,s={},c=[{value:"Benefits",id:"toc-benefits",level:2},{value:"Quick Start",id:"toc-quick-start",level:2},{value:"Common Interface Files",id:"toc-common-interface-file",level:2},{value:"Common Interface File in <code>.js.flow</code>",id:"toc-common-interface-file-in-js-flow",level:3},{value:"Common Interface File in <code>.js</code>",id:"toc-common-interface-file-in-js",level:3}];function d(e){const t={a:"a",admonition:"admonition",code:"code",h2:"h2",h3:"h3",p:"p",pre:"pre",...(0,a.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(t.admonition,{type:"caution",children:(0,i.jsx)(t.p,{children:"The feature is still experimental. Behaviors might change in the future."})}),"\n",(0,i.jsx)(t.h2,{id:"toc-benefits",children:"Benefits"}),"\n",(0,i.jsxs)(t.p,{children:["React Native supports conditional bundling of files with ",(0,i.jsx)(t.a,{href:"https://reactnative.dev/docs/platform-specific-code#platform-specific-extensions",children:"platform specific extensions"}),". For example, if you have different implementations of an Image component for iOS and Android, you can have an ",(0,i.jsx)(t.code,{children:"Image.ios.js"})," file and ",(0,i.jsx)(t.code,{children:"Image.android.js"})," file, and an import of Image can be resolved to either file based on the platform you are targeting."]}),"\n",(0,i.jsx)(t.p,{children:"These platform specific files live under the same repository, but it would normally require two flowconfigs to check them like the following setup:"}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-toml",metastring:"title=.flowconfig",children:"; for ios\n[ignore]\n.*\\.android\\.js$\n[options]\nmodule.file_ext=.js\nmodule.file_ext=.ios.js\n"})}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-toml",metastring:"title=.flowconfig.android",children:"; for android\n[ignore]\n; Ignore other platform suffixes\n.*\\.ios\\.js$\n[options]\nmodule.file_ext=.js\nmodule.file_ext=.android.js\n"})}),"\n",(0,i.jsx)(t.p,{children:"Flow's optional React Native multi-platform support allows you to check your entire project with mixed platforms under a single Flow root, so that during the development of a module with both .ios and .android files, you no longer have to run both Flow servers and constantly switch between different servers to see type errors on different platforms."}),"\n",(0,i.jsx)(t.h2,{id:"toc-quick-start",children:"Quick Start"}),"\n",(0,i.jsxs)(t.p,{children:["You can start by deleting the flowconfig for all other platforms, deleting all the platform specific configs in the only remaining flowconfig, and add the following new lines to the ",(0,i.jsx)(t.code,{children:"options"})," section:"]}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{children:"experimental.multi_platform=true\nexperimental.multi_platform.extensions=.ios\nexperimental.multi_platform.extensions=.android\n"})}),"\n",(0,i.jsxs)(t.p,{children:["For example, these are the required changes for the ",(0,i.jsx)(t.code,{children:".flowconfig"})," example above:"]}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-diff",metastring:"title=.flowconfig",children:"[ignore]\n- .*\\.android\\.js$\n[options]\nmodule.file_ext=.js\n- module.file_ext=.ios.js\n+ experimental.multi_platform=true\n+ experimental.multi_platform.extensions=.ios\n+ experimental.multi_platform.extensions=.android\n"})}),"\n",(0,i.jsx)(t.p,{children:"After enabling the new configurations, there will likely be new errors. The sections below explain the additional rules that Flow imposes to check a multiplatform React Native project."}),"\n",(0,i.jsx)(t.h2,{id:"toc-common-interface-file",children:"Common Interface Files"}),"\n",(0,i.jsxs)(t.p,{children:["Suppose you have a file that imports the ",(0,i.jsx)(t.code,{children:"Image"})," module, but ",(0,i.jsx)(t.code,{children:"Image"})," module has different iOS and Android implementations as follows:"]}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-jsx",metastring:"title=MyReactNativeApp.js",children:"import * as React from 'react';\nimport Image from './Image';\n\n<Image src=\"/hello.png\" />;\n<Image src=\"/world.png\" lazyLoading={true} />;\n"})}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-jsx",metastring:"title=Image.ios.js",children:"import * as React from 'react';\n\ntype Props = { src: string, lazyLoading?: boolean };\n\nexport default function Image(props: Props): React.Node { /* ... */ }\n"})}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-jsx",metastring:"title=Image.android.js",children:"import * as React from 'react';\n\ntype Props = { src: string, lazyLoading: boolean };\n\nexport default class Image extends React.Components<Props> {\n  static defaultProps: { lazyLoading: boolean } = { lazyLoading: false };\n  render(): React.Node { /* ... */ }\n}\n"})}),"\n",(0,i.jsxs)(t.p,{children:["When you enabled multiplatform support, you will likely see that error that the ",(0,i.jsx)(t.code,{children:"./Image"})," module cannot be resolved. To fix the error, you need to create a common interface file under the same directory:"]}),"\n",(0,i.jsxs)(t.h3,{id:"toc-common-interface-file-in-js-flow",children:["Common Interface File in ",(0,i.jsx)(t.code,{children:".js.flow"})]}),"\n",(0,i.jsxs)(t.p,{children:["One option is to write a common interface file in ",(0,i.jsx)(t.code,{children:".js.flow"}),":"]}),"\n",(0,i.jsxs)(t.p,{children:["With ",(0,i.jsx)(t.a,{href:"../component-types/",children:"Component Types"})]}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-jsx",metastring:"title=Image.js.flow",children:"import * as React from 'react';\n\ntype Props = { src: string, lazyLoading?: boolean };\n\ndeclare const Image: component(...Props);\nexport default Image;\n"})}),"\n",(0,i.jsxs)(t.p,{children:["With ",(0,i.jsx)(t.a,{href:"../types#toc-react-componenttype",children:"React.ComponentType"})]}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-jsx",metastring:"title=Image.js.flow",children:"import * as React from 'react';\n\ntype Props = { src: string, lazyLoading?: boolean };\n\ndeclare const Image: React.ComponentType<Props>;\nexport default Image;\n"})}),"\n",(0,i.jsxs)(t.p,{children:["Flow will ensure that the module types of both ",(0,i.jsx)(t.code,{children:"Image.ios.js"})," and ",(0,i.jsx)(t.code,{children:"./Image.android.js"})," are subtype of the module type of ",(0,i.jsx)(t.code,{children:"./Image.js.flow"}),". Flow will also ensure that there exists an implementation for each platform you declared in your ",(0,i.jsx)(t.code,{children:".flowconfig"}),"."]}),"\n",(0,i.jsxs)(t.h3,{id:"toc-common-interface-file-in-js",children:["Common Interface File in ",(0,i.jsx)(t.code,{children:".js"})]}),"\n",(0,i.jsxs)(t.p,{children:["Sometimes you might target desktop platforms in addition to iOS and Android, and you only have a special implementation for one platform, and all the other platforms will use the fallback implementation in a ",(0,i.jsx)(t.code,{children:".js"})," file. For example:"]}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-jsx",metastring:"title=Image.js",children:"import * as React from 'react';\nimport DefaultImage from 'react-native/Libraries/Image';\n\nexport default DefaultImage;\n"})}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-jsx",metastring:"title=Image.ios.js",children:"import * as React from 'react';\n\ntype Props = { src: string, lazyLoading: boolean };\n\nexport default function Image(props: Props): React.Node {\n  // Custom implementation to take advantage of some unique iOS capabilities\n}\n"})}),"\n",(0,i.jsxs)(t.p,{children:["In this case, Flow will use the ",(0,i.jsx)(t.code,{children:".js"})," file as the common interface file, and check all other platform-specific implementation files' against the ",(0,i.jsx)(t.code,{children:".js"})," file. Since the ",(0,i.jsx)(t.code,{children:".js"})," file is already a fallback implementation, Flow will no longer require that platform-specific implementation files exist for all platforms."]})]})}function m(e={}){const{wrapper:t}={...(0,a.R)(),...e.components};return t?(0,i.jsx)(t,{...e,children:(0,i.jsx)(d,{...e})}):d(e)}}}]);