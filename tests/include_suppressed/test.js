// @flow

123 as string; // Normal error

// $FlowFixMe[incompatible-type]
123 as string; // Suppressed error

// $FlowFixMe - unused suppression comment
