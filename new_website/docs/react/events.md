---
title: Event Handling
slug: /react/events
---

In the [React docs "Handling Events" section][] a few different recommendations
are provided on how to define event handlers. If you are using Flow we recommend
that you use [property initializer syntax][] as it is the easiest to statically
type. Property initializer syntax looks like this:

[React docs "Handling Events" section]: https://facebook.github.io/react/docs/handling-events.html
[property initializer syntax]: https://babeljs.io/docs/plugins/transform-class-properties/

```js
class MyComponent extends React.Component<{}> {
  handleClick = event => { /* ... */ };
}
```

To type event handlers you may use the `SyntheticEvent<T>` types like this:

```js
import * as React from 'react';

class MyComponent extends React.Component<{}, { count: number }> {
  handleClick = (event: SyntheticEvent<HTMLButtonElement>) => {
    // To access your button instance use `event.currentTarget`.
    (event.currentTarget: HTMLButtonElement);

    this.setState(prevState => ({
      count: prevState.count + 1,
    }));
  };

  render() {
    return (
      <div>
        <p>Count: {this.state.count}</p>
        <button onClick={this.handleClick}>
          Increment
        </button>
      </div>
    );
  }
}
```

There are also more specific synthetic event types like
`SyntheticKeyboardEvent<T>`, `SyntheticMouseEvent<T>`, or
`SyntheticTouchEvent<T>`. The `SyntheticEvent<T>` types all take a single type
argument. The type of the HTML element the event handler was placed on.

If you don't want to add the type of your element instance you can also use
`SyntheticEvent` with *no* type arguments like so: `SyntheticEvent<>`.

> **Note:** To get the element instance, like `HTMLButtonElement` in the example
> above, it is a common mistake to use `event.target` instead of
> `event.currentTarget`. The reason why you want to use `event.currentTarget` is
> that `event.target` may be the wrong element due to [event propagation][].

[event propagation]: https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Examples#Example_5:_Event_Propagation

> **Note:** React uses its own event system so it is important to use the
> `SyntheticEvent` types instead of the DOM types such as `Event`,
> `KeyboardEvent`, and `MouseEvent`.

The `SyntheticEvent<T>` types that React provides and the DOM events they are
related to are:

- `SyntheticEvent<T>` for [Event](https://developer.mozilla.org/en-US/docs/Web/API/Event)
- `SyntheticAnimationEvent<T>` for [AnimationEvent](https://developer.mozilla.org/en-US/docs/Web/API/AnimationEvent)
- `SyntheticCompositionEvent<T>` for [CompositionEvent](https://developer.mozilla.org/en-US/docs/Web/API/CompositionEvent)
- `SyntheticInputEvent<T>` for [InputEvent](https://developer.mozilla.org/en-US/docs/Web/API/InputEvent)
- `SyntheticUIEvent<T>` for [UIEvent](https://developer.mozilla.org/en-US/docs/Web/API/UIEvent)
- `SyntheticFocusEvent<T>` for [FocusEvent](https://developer.mozilla.org/en-US/docs/Web/API/FocusEvent)
- `SyntheticKeyboardEvent<T>` for [KeyboardEvent](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent)
- `SyntheticMouseEvent<T>` for [MouseEvent](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent)
- `SyntheticDragEvent<T>` for [DragEvent](https://developer.mozilla.org/en-US/docs/Web/API/DragEvent)
- `SyntheticWheelEvent<T>` for [WheelEvent](https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent)
- `SyntheticTouchEvent<T>` for [TouchEvent](https://developer.mozilla.org/en-US/docs/Web/API/TouchEvent)
- `SyntheticTransitionEvent<T>` for [TransitionEvent](https://developer.mozilla.org/en-US/docs/Web/API/TransitionEvent)
