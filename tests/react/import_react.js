/* @flow */

import react from "react";
import {Component} from "react";

var a: Component<any> = new react.Component();
var b: number = new react.Component(); // Error: ReactComponent ~> number
