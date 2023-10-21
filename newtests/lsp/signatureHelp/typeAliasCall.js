// @flow

/**
 * function alias docs
 * @param x is a string
 */
type F = (x: string) => void;
/**
 * variable declaration
 * @param x is a string
 */
declare const g: F;

g(
  /* here */
)
