// Uses computed property keys imported from another module (remote bindings)
import {REMOTE_STR_KEY, REMOTE_NUM_KEY} from './computed_property_key_constants';

// Object type with remote string key
export type WithRemoteStrKey = {[REMOTE_STR_KEY]: number};

// Object type with remote number key
export type WithRemoteNumKey = {[REMOTE_NUM_KEY]: string};

// Interface with remote key
export interface WithRemoteIface {
  [REMOTE_STR_KEY]: boolean;
}

// Declare class with remote key
export declare class WithRemoteClass {
  [REMOTE_NUM_KEY]: boolean;
}
