class MyComponent {
  onEvent = (event: SyntheticEvent) => console.log('yo');
  onAnimationEvent = (event: SyntheticAnimationEvent) => console.log('yo');
  onClipboardEvent = (event: SyntheticClipboardEvent) => console.log('yo');
  onCompositionEvent = (event: SyntheticCompositionEvent) => console.log('yo');
  onInputEvent = (event: SyntheticInputEvent) => console.log('yo');
  onUIEvent = (event: SyntheticUIEvent) => console.log('yo');
  onFocusEvent = (event: SyntheticFocusEvent) => console.log('yo');
  onKeyboardEvent = (event: SyntheticKeyboardEvent) => console.log('yo');
  onMouseEvent = (event: SyntheticMouseEvent) => console.log('yo');
  onDragEvent = (event: SyntheticDragEvent) => console.log('yo');
  onWheelEvent = (event: SyntheticWheelEvent) => console.log('yo');
  onTouchEvent = (event: SyntheticTouchEvent) => console.log('yo');
  onTransitionEvent = (event: SyntheticTransitionEvent) => console.log('yo');
  onNopeEvent = (event: SyntheticNopeEvent) => console.log('nope');
}
