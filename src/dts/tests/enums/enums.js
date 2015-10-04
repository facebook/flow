// @flow
var x = Color.R;

var y: string = "";

switch (x) {
    case Color.B:
        y = Color.R;
    case Color.R:
        y = "this is a string";
}
