function x() {}
declare const y: () => void;

x.length as empty; // ERROR
y.length as empty; // ERROR

x.name as empty; // ERROR
y.name as empty; // ERROR

x.length = 0; // ERROR
y.length = 0; // ERROR

x.name = 'foo'; // ERROR
y.name = 'foo'; // ERROR
