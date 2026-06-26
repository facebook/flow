// Regression test for a Rust checker panic when a homomorphic mapped type
// recurses into primitive fields through react-hook-form's FieldErrors shape.

declare class Blob {}

type BrowserNativeObject = Blob;
type FieldValues = {...};

type FieldError = {
  message?: string,
  ...
};

type GlobalError = FieldError;

type DeepRequired<T> = T extends BrowserNativeObject
  ? T
  : {[K in keyof T]-?: NonNullable<DeepRequired<T[K]>>};

type FieldErrorsImpl<T extends FieldValues = FieldValues> = {
  [K in keyof T]?: T[K] extends BrowserNativeObject
    ? FieldError
    : K extends 'root' | `root.${string}` // ERROR - template literal types are unsupported
      ? GlobalError
      : T[K] extends {...}
        ? FieldError & FieldErrorsImpl<T[K]>
        : FieldError,
};

type FieldErrors<T extends FieldValues = FieldValues> = Partial<
  FieldErrorsImpl<DeepRequired<T>>,
> & {
  root?: ({[key: string]: GlobalError, ...} & GlobalError),
  ...
};

declare function useForm<T extends FieldValues>(): {
  formState: {
    errors: FieldErrors<T>,
    ...
  },
  ...
};

type LoginForm = {
  name?: string,
  enabled?: boolean,
  nested: {
    count?: number,
    ...
  },
  ...
};

const form = useForm<LoginForm>();
form.formState.errors.name?.message as ?string; // ERROR
form.formState.errors.enabled?.message as ?string; // OK
form.formState.errors.nested?.count?.message as ?string; // OK
