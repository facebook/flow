type Alert = {severity: 'RED' | 'YELLOW'};
type Count = {value: 1 | 2};

declare class List<out T> {
  static <T>(input: ?$Iterable<T, void, void>): List<T>;
  static <T>(input: ?IteratorObject<T>): List<T>;
  get(index: number): T;
}

function alerts(values: ?ReadonlyArray<?string>): List<Alert> {
  return List(
    values
      ?.map(severityValue => ({
        severity:
          severityValue !== 'RED' && severityValue !== 'YELLOW'
            ? ('YELLOW' as const)
            : severityValue,
      }))
      .filter(Boolean) ?? [],
  );
}

function invalidAlerts(values: ?ReadonlyArray<?string>): List<Alert> {
  return List(
    values
      ?.map(severityValue => ({
        severity:
          severityValue !== 'RED' && severityValue !== 'GREEN'
            ? ('YELLOW' as const)
            : severityValue,
      }))
      .filter(Boolean) ?? [],
  ); // Error: 'GREEN' is incompatible with 'RED' | 'YELLOW'.
}

function counts(values: ?ReadonlyArray<?number>): List<Count> {
  return List(
    values
      ?.map(value => ({
        value: value !== 1 && value !== 2 ? (1 as const) : value,
      }))
      .filter(Boolean) ?? [],
  );
}

function invalidCounts(values: ?ReadonlyArray<?number>): List<Count> {
  return List(
    values
      ?.map(value => ({
        value: value !== 1 && value !== 3 ? (1 as const) : value,
      }))
      .filter(Boolean) ?? [],
  ); // Error: 3 is incompatible with 1 | 2.
}
