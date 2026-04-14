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
  CallableValue as mixed;
};

const useManyB = ({
  CallableValue,
}: {
  CallableValue: CallableValue,
}): void => {
  CallableValue as mixed;
};

const useManyC = ({
  CallableValue,
}: {
  CallableValue: CallableValue,
}): void => {
  CallableValue as mixed;
};

useProperty;
useDestructuredProperty;
useManyA;
useManyB;
useManyC;
