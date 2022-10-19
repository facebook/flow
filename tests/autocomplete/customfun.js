// @flow

declare function idx<IdxObject, IdxResult>(object: IdxObject, f: (_: $Facebookism$IdxWrapper<IdxObject>) => IdxResult): ?$Facebookism$IdxUnwrapper<IdxResult>;
declare var objectGetPrototypeOf: Object$GetPrototypeOf
declare var objectAssign: Object$Assign
   //
//^
