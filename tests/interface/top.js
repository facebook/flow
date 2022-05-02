declare var x: mixed;

(x: interface {}); // ERROR

(undefined: interface {}); // ERROR
(null: interface {}); // ERROR

(1: interface {}); // ERROR
(true: interface {}); // ERROR
