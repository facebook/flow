// @flow

// Type alias for component properties
type UserCardProps = {
  name: string,
  age?: number,
  email: string,
  isActive: boolean,
};

// Type alias for generic component properties
type DataDisplayProps<T> = {
  data: T,
  formatter: (T) => string,
  showLabel?: boolean,
};

// Type alias for product item properties
type ProductItemProps = {
  id: string,
  title: string,
  price: number,
  inStock: boolean,
  category?: string,
};

export type {UserCardProps, DataDisplayProps, ProductItemProps};
