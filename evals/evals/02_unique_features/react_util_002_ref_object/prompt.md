Implement the `selectAll` helper used by the `SearchField` component already in `main.js`.

`selectAll` takes the ref container produced by the `useRef` hook: the object that holds a nullable `HTMLInputElement` in its `current` field. When the element is present, the helper focuses it and selects its entire text (set the selection range from `0` to the value's length). Annotate the parameter with the type of that ref container.
