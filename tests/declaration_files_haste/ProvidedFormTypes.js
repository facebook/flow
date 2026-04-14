import type ProvidedParams from 'ProvidedParams';

export type FormProps = Readonly<{
  onChange: ProvidedParams => void,
  params: ProvidedParams,
}>;
