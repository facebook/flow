import {action} from './action_helper';

action(
  {a: 1},
  ({a, b}) => b,
) as ({b: string}) => string; // ERROR
