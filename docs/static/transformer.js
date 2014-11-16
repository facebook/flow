var TEXT = "\
/* @flow */\n\
var x: number = 0;\n\
var f = function(a: Array<string | number>): boolean {\n\
  return false;\n\
}\
";
function transformer(code) {
  return JSXTransformer.transform(code, {harmony: true, stripTypes: true}).code;
}
var CompilerPlayground = React.createClass({displayName: 'CompilerPlayground',
  render: function() {
    return (
      React.createElement("div", null,
        React.createElement(ReactPlayground, {
          codeText: TEXT,
          renderCode: true,
          transformer: transformer,
          showCompiledJSTab: false
        })
      )
    );
  },
});
React.render(
  React.createElement(CompilerPlayground, null),
  document.getElementById('jsxCompiler')
);
