import typeof CallableValue from './CallableValue';

const useProperty = (payload: {
  CallableValue: CallableValue,
}): void => {
  payload;
};

const useDestructuredProperty = ({
  CallableValue,
}: {
  CallableValue: CallableValue,
}): void => {
  CallableValue;
};

const useManyA = ({
  CallableValue,
}: {
  CallableValue: CallableValue,
}): void => {
  CallableValue as unknown;
};

const useManyB = ({
  CallableValue,
}: {
  CallableValue: CallableValue,
}): void => {
  CallableValue as unknown;
};

const useManyC = ({
  CallableValue,
}: {
  CallableValue: CallableValue,
}): void => {
  CallableValue as unknown;
};

useProperty;
useDestructuredProperty;
useManyA;
useManyB;
useManyC;
