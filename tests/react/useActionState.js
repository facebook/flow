import * as React from 'react'

declare function addToCart(prevState: ?string): Promise<?string>;

function AddToCartForm() {
  const [message, formAction, isPending] = React.useActionState(addToCart, null); // OK
  return (
    <form action={formAction}>
      <button type="submit">Add to Cart</button>
      {isPending ? "Loading..." : message}
    </form>
  );
}
