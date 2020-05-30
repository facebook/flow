// @flow

// Error on named exported functions
export function named1() { this; }
export const named2 = function() { this; }
export const named3 = () => { this; }

// Error on exported functions in exports list
function exported1() { this; }
const exported2 = function() { this; };
const exported3 = () => { this; };

export { exported1, exported2, exported3 }

// Do not error since these functions are not exported
function internal1() { this; }
const internal2 = function() { this; };
const internal3 = () => { this; };

// Do not error if this is within a class
export function thisWithinClass() {
  class C {
    classMethod() { this; }
  }
}
