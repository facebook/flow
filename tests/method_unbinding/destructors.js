// @flow

class A {
    m() : void {}
}


declare var m : $PropertyType<A, "m">; // ok bc this does not actually unbind anything at runtime

(m : () => void); // use required to force EvalT to evaluate
