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

