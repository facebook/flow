
var app = require('JSX');

app.setProps({y:42}); // error, y:number but foo expects string in App.react
app.setState({z:42}); // error, z:number but foo expects string in App.react

function bar(x:number) { }
bar(app.props.children); // error, app.props.children: mixed ~> number
