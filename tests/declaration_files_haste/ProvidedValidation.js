import type ProvidedParams from 'ProvidedParams';

export function validateProvidedParams(
  params: ProvidedParams,
): Readonly<{errors: ReadonlyArray<string>, valid: boolean}> {
  const errors: Array<string> = [];

  if (params.get('tags').size === 0) {
    errors.push('No tags were provided.');
  }

  return {
    valid: !errors.length,
    errors,
  };
}
