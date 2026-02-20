// @flow

import * as React from 'react';
import type {UserCardProps, DataDisplayProps, ProductItemProps, DocumentedProps} from './types';

// Basic component
component BasicComponent(title: string, count: number) {
  return <div>{title}: {count}</div>;
}

// Component with optional props
component OptionalProps(required: string, optional?: number) {
  return <div>{required} {optional ?? 0}</div>;
}

// Generic component
component GenericComponent<T>(data: T, formatter: (T) => string) {
  return <div>{formatter(data)}</div>;
}

// Component with rest props (inexact object)
component InexactRest(foo: number, ...rest: {bar: string, ...}) {
  return <div>Foo: {foo}, Bar: {rest.bar}</div>;
}

// Component with rest props (indexed)
component IndexedRest(foo: number, ...rest: {[string]: number, ...}) {
  return <div>Foo: {foo}</div>;
}

// Component with named props using 'as'
component NamedProps('data-id' as dataId: string, 'aria-label' as ariaLabel: string) {
  return <div data-id={dataId} aria-label={ariaLabel}>Named props</div>;
}

// Component using imported exact props type alias
component UserCard(...props: UserCardProps) {
  return (
    <div>
      <h3>{props.name}</h3>
      <p>Email: {props.email}</p>
      {props.age && <p>Age: {props.age}</p>}
      <p>Status: {props.isActive ? 'Active' : 'Inactive'}</p>
    </div>
  );
}

// Generic component using imported exact props type alias
component DataDisplay<T>(...props: DataDisplayProps<T>) {
  return (
    <div>
      {props.showLabel && <label>Data:</label>}
      <span>{props.formatter(props.data)}</span>
    </div>
  );
}

// Component using imported exact props type alias with direct props
component ProductItem(...props: ProductItemProps) {
  return (
    <div>
      <h4>{props.title}</h4>
      <p>Price: ${props.price}</p>
      <p>In Stock: {props.inStock ? 'Yes' : 'No'}</p>
      {props.category && <p>Category: {props.category}</p>}
    </div>
  );
}

// Component that combines explicit props with spread props from imported type
component UserProfile(
  className: string,
  showActions: boolean = true,
  ...userProps: UserCardProps
) {
  return (
    <div className={className}>
      <UserCard {...userProps} />
      {showActions && (
        <div>
          <button>Edit</button>
          <button>Delete</button>
        </div>
      )}
    </div>
  );
}

/**
 * A component that displays a notification message.
 *
 * @param message - The notification message to display
 * @param severity - The type of notification (info, warning, error, success)
 * @param onDismiss - Callback function called when notification is dismissed
 */
component NotificationWithDocblock(
  message: string,
  severity: 'info' | 'warning' | 'error' | 'success' = 'info',
  onDismiss?: () => void
) {
  return <div>{message}</div>;
}

// Component with constrained generic type parameter
component ConstrainedGeneric<T: {id: string, label: string}>(items: Array<T>, selected?: T) {
  return <div>{items.length}</div>;
}

// Export components for external use
export {
  BasicComponent,
  OptionalProps,
  GenericComponent,
  ConstrainedGeneric,
  InexactRest,
  IndexedRest,
  NamedProps,
  UserCard,
  DataDisplay,
  ProductItem,
  UserProfile,
  NotificationWithDocblock,
};

// Component using props with per-field JSDoc documentation
component DocumentedComponent(...props: DocumentedProps) {
  return (
    <div>
      <span>{props.displayName}</span>
      <span>{props.userId}</span>
      {props.isOnline && <span>Online</span>}
    </div>
  );
}

export {DocumentedComponent};
