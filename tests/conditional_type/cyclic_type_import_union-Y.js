// @flow

type ValidatorFunction = {
  (): boolean,
  errors?: ?Array<string>,
  schema?: string,
  ...
};

export default class Y {
  compile(): ValidatorFunction {
    return () => true;
  }
}
