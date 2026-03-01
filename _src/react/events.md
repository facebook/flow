---
title: Event Handling
slug: /react/events
---

The [React docs for handling events](https://react.dev/learn/responding-to-events) show how an event handler can be attached to
a React element. To type these event handlers you may use the `SyntheticEvent<T>`
types like this:

```js flow-check
import {useState} from 'react';
import * as React from 'react';

function MyComponent(): React.Node {
  const [state, setState] = useState({count: 0});

  const handleClick = (event: SyntheticEvent<HTMLButtonElement>) => {
    // To access your button instance use `event.currentTarget`.
    event.currentTarget as HTMLButtonElement;

    setState(prevState => ({
      count: prevState.count + 1,
    }));
  };

  return (
    <div>
      <p>Count: {state.count}</p>
      <button onClick={handleClick}>Increment</button>
    </div>
  );
}
```

There are also more specific synthetic event types like
`SyntheticKeyboardEvent<T>`, `SyntheticMouseEvent<T>`, or
`SyntheticTouchEvent<T>`. The `SyntheticEvent<T>` types all take a single type
argument: the type of the HTML element the event handler was placed on.

If you don't want to add the type of your element instance you can also use
`SyntheticEvent` with *no* type arguments like so: `SyntheticEvent<>`.

> **Note:** To get the element instance, like `HTMLButtonElement` in the example
> above, it is a common mistake to use `event.target` instead of
> `event.currentTarget`. The reason you want to use `event.currentTarget` is
> that `event.target` may be the wrong element due to [event propagation][].

[event propagation]: https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Examples#example_5_event_propagation

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
