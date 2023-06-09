import * as React from 'react';

type SourceObj = { foo: string, bar: number };

let f = () => {
  declare const Comp: React.ComponentType<SourceObj>;
  <Comp foo={''} bar={3} />;
//       ^
}

f = () => {
  declare const Comp: React.ComponentType<$ReadOnly<SourceObj>>;
  <Comp foo={''} bar={3} />;
//       ^
}

f = () => {
  declare const Comp: React.ComponentType<$Diff<SourceObj, {bar: number}>>;
  <Comp foo={''} />;
//       ^
}

f = () => {
  declare const Comp: React.ComponentType<$Rest<SourceObj, {bar: number}>>;
  <Comp foo={''} />;
//       ^
}

f = () => {
  declare const Comp: React.ComponentType<$ObjMap<SourceObj, ((string)=>number) & ((number)=>string)>>;
  <Comp foo={3} bar={''} />;
//       ^
}

f = () => {
  declare const Comp: React.ComponentType<$ObjMapConst<SourceObj, string>>;
  <Comp foo={''} bar={''} />;
//       ^
}

f = () => {
  declare const Comp: React.ComponentType<{+[key in keyof SourceObj]: SourceObj[key]}>;
  <Comp foo={''} bar={3} />;
//       ^
}

f = () => {
  declare const BaseComp: React.ComponentType<SourceObj>;
  type $RelayProps<T> = $ObjMap<
    $Diff<T, {relayProp: string | void}>,
    & (<T: { +$fragmentType: empty, ... }>( T) =>  T)
    & (<T>(T) => T),
  >;
  declare const Comp: React.ComponentType<$RelayProps<React.ElementConfig<typeof BaseComp>>>;
  <Comp foo={''} bar={3} />;
//       ^
}

f = () => {
  type MapType<T> = $ReadOnly<T>
  declare const Comp: React.ComponentType<MapType<SourceObj>>;
  <Comp foo={''} bar={3} />;
//       ^
}

f = () => {
  type MapType<T> = $Diff<T, {bar: number}>;
  declare const Comp: React.ComponentType<MapType<SourceObj>>;
  <Comp foo={''} />;
//       ^
}

f = () => {
  type MapType<T> = $Rest<T, {bar: number}>;
  declare const Comp: React.ComponentType<MapType<SourceObj>>;
  <Comp foo={''} />;
//       ^
}

f = () => {
  type MapType<T> = $ObjMap<T, ((string)=>number) & ((number)=>string)>
  declare const Comp: React.ComponentType<MapType<SourceObj>>;
  <Comp foo={3} bar={''} />;
//       ^
}

f = () => {
  type MapType<T> = $ObjMapConst<T, string>
  declare const Comp: React.ComponentType<MapType<SourceObj>>;
  <Comp foo={''} bar={''} />;
//       ^
}

f = () => {
  type MapType<T> = {+[key in keyof T]: SourceObj[key]};
  declare const Comp: React.ComponentType<MapType<SourceObj>>;
  <Comp foo={''} bar={3} />;
//       ^
}
