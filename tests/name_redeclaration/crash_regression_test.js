// @flow

// The following code used to cause a crash, but they have been fixed now.

function missing_provider_crash_fixed() {
  let x = 0;
  let x: number = 0; // Error
  x = 1;

  let y = 0;
  function y() {} // Error
}
