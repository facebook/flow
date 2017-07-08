var React = require('React');

var text = 'hello';
var div = React.createElement("div");
var children = [text, div, null];
var callback = (child: React$Child<*>, i) => i;

var mapArray: Array<number> = React.Children.map(children, callback);
var mapArraywithCtx: Array<number> = React.Children.map(children, callback, {});
var mapText: Array<number> = React.Children.map(text, callback);
var mapElement: Array<number> = React.Children.map(div, callback);
var mapNull: Array<number> = React.Children.map(null, callback);

var mapWrongType: Array<string> = React.Children.map(children, callback); // error: number -> string
React.Children.map(children, (child: React$Element<*>, i) => i); // error: React$Element -> number | string
React.Children.map(children); // error: missing argument


React.Children.forEach(children, callback);
React.Children.forEach(children, callback, {});
React.Children.forEach(text, callback);
React.Children.forEach(div, callback);
React.Children.forEach(null, callback);

React.Children.forEach(children, (child: React$Element<*>, i) => i); // error: React$Element -> number | string
React.Children.forEach(children); // error: missing argument


var countArray: number = React.Children.count(children);
var countText: number = React.Children.count(text);
var countElement: number = React.Children.count(div);
var countNull: number = React.Children.count(null);

var countWrongType: string = React.Children.count(children); // error: number -> string


var onlyArray: React$Element<*> = React.Children.only(children); // throws in runtime
var onlyText: React$Element<*> = React.Children.only(text); // throws in runtime
var onlyElement: React$Element<*> = React.Children.only(children);
var onlyNull: React$Element<*> = React.Children.only(children); // throws in runtime

var onlyWrongType: string = React.Children.only(text); // error: React$Element -> string


var toArrayArray: Array<React$Child<*>> = React.Children.toArray(children);
var toArrayText: Array<React$Child<*>> = React.Children.toArray(text);
var toArrayElement: Array<React$Child<*>> = React.Children.toArray(div);
var toArrayNull: Array<React$Child<*>> = React.Children.toArray(null);

var toArrayWrongType: Array<React$Element<*>> = React.Children.toArray(children); // error: React$Element -> number | string
