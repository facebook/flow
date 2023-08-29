/* @flow */

if (navigator.usb) {
  navigator.usb.getDevices(); // correct
}

if (navigator.usb) {
  navigator.usb.requestDevice({
    filters: [{}],
  }); // correct
}
