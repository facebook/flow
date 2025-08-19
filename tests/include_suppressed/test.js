// @flow

(123: string); // Normal error

// $FlowFixMe[incompatible-type]
(123: string); // Suppressed error

// $FlowFixMe - unused suppression comment
