The code in `main.js` has a type error. Fix it.

`Picker.js` (which you should not modify) default-exports a generic React component. In `main.js`, the `PickerComponent` type must remain the type of whatever component `Picker.js` exports by default — derive it from that module rather than rewriting it by hand. `register` must still accept a value of that component type, and `count` must keep returning how many have been registered.
