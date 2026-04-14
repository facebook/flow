import actionNamespace from './action_namespace';

actionNamespace.tools.action(
  {a: 1},
  ({a, b}) => b,
) as ({b: string}) => string; // ERROR
