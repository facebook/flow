Write Flow code in `main.js` for a video player with a restart control.

Write a `Player` component that takes a `src: string`. It renders a `<video src={src}>` and keeps a ref to that video element so it can control it imperatively. Derive the type of the value stored in the ref from the `'video'` element itself rather than writing the DOM element class by hand.

Add a 'Restart muted' `<button>` whose click handler, when the element is available, resets the video's `currentTime` to `0` and sets `muted` to `true`.
