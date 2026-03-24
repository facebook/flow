import type {WithRemoteStrKey, WithRemoteNumKey, WithRemoteIface, WithRemoteClass} from './computed_property_key_remote';

// Remote string key resolves to a named property
declare const strObj: WithRemoteStrKey;
strObj.remoteProp as number; // OK
strObj.remoteProp as string; // ERROR

// Remote number key resolves to a named property
declare const numObj: WithRemoteNumKey;
numObj[99] as string; // OK
numObj[99] as number; // ERROR

// Remote key in interface
declare const ifaceObj: WithRemoteIface;
ifaceObj.remoteProp as boolean; // OK
ifaceObj.remoteProp as string; // ERROR

// Remote key in declare class
declare const classObj: WithRemoteClass;
classObj[99] as boolean; // OK
classObj[99] as string; // ERROR
