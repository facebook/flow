import {useState} from 'react';

function Component() {
  useState(); // OK
}

function _Component() {
  useState(); // OK
}

function f() {
  useState(); // ERROR
}
