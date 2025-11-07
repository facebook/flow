// @flow

// Exported types with @flowCustomError

/**
 * @flowCustomError
 * @description custom desc
 */
export type Email = string;

/**
 * @flowCustomError
 * @description custom desc
 */
export type Age = number;

/**
 * @flowCustomError
 * @description custom desc
 */
export type UserProfile = {
  email: Email,
  age: Age,
  displayName: string,
};

/**
 * @flowCustomError
 * @description custom desc
 */
export type ValidatedInput<T> = {
  value: T,
  isValid: boolean,
  errors: Array<string>,
};

/**
 * @flowCustomError
 * @description custom desc
 */
export type ApiResponse<T, E = string> =
  | { status: "success", data: T }
  | { status: "error", error: E };

/**
 * @flowCustomError
 * @description custom desc
 */
export type ID = string | number;

// Normal exported type without custom error
export type NormalType = {
  field: string,
};

/**
 * @flowCustomError
 * @description custom desc
 */
export type Coordinate = {
  x: number,
  y: number,
  ...
};

/**
 * @flowCustomError
 * @description custom desc
 */
export type Point3D = Coordinate & { z: number, ... };

/**
 * @flowCustomError
 * @description custom desc
 */
export type Color = "red" | "green" | "blue";

/**
 * @flowCustomError
 * @description custom desc
 */
export type ColorMap<T> = {
  red: T,
  green: T,
  blue: T,
};

/**
 * @flowCustomError
 * @description custom desc
 */
export type Callback<T> = (value: T) => void;

/**
 * @flowCustomError
 * @description custom desc
 */
export type Optional<T> = T | null | void;

/**
 * @flowCustomError
 * @description custom desc
 */
export type ReadonlyRecord<K: string, V> = {
  +[key: K]: V,
};
