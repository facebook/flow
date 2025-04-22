type Annotations = Partial<{
  string: ?{[string]: ?string, ...},
  int: ?{[string]: ?number, ...},
}>;


function addAnnotations(
  target: Annotations,
  source: Annotations,
): void {
  Object.keys(source).forEach(annotationTypeName => {
    target[annotationTypeName] = Object.assign( // error
      target[annotationTypeName],
      source[annotationTypeName],
    );
  });
}
