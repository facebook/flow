import {key1, key2} from './keys';

export class E {
  [key1]: number = 1;
  [key2](): string {
    return 's';
  }
}
