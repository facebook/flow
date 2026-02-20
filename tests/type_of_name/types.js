// @flow

// Type alias for component properties
export type UserCardProps = {
  name: string,
  age?: number,
  email: string,
  isActive: boolean,
};

// Type alias for generic component properties
export type DataDisplayProps<T> = {
  data: T,
  formatter: (T) => string,
  showLabel?: boolean,
};

// Type alias for product item properties
export type ProductItemProps = {
  id: string,
  title: string,
  price: number,
  inStock: boolean,
  category?: string,
};

/**
 * Props for a documented component with per-field JSDoc.
 */
export type DocumentedProps = {
  /** The user's display name */
  displayName: string,
  /** The user's unique identifier */
  userId: string,
  /** Whether the user is currently online */
  isOnline?: boolean,
};

// Types for testing ref expansion heuristics

// Simple union type (should expand fully)
type TextWrap = 'wrap' | 'nowrap';

// Larger union type (should truncate)
type SevenColors = 'red' | 'orange' | 'yellow' | 'green' | 'blue' | 'indigo' | 'violet';

// Large object type (should show field count)
type LargeObjType = {
  field1: string,
  field2: number,
  field3: boolean,
  field4: string,
  field5: number,
  field6: boolean,
};

// Function type alias
type Formatter = (input: string) => string;

// Component using various ref types for expansion testing
component RefExpansionTest(
  textWrap: TextWrap,
  colors: SevenColors,
  config: LargeObjType,
  fmt: Formatter,
) {
  return null;
}

export type {TextWrap, SevenColors, LargeObjType, Formatter};
export {RefExpansionTest};
