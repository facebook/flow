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
  declare const Comp: React.ComponentType<{+[key in keyof SourceObj]: SourceObj[key]}>;
  <Comp foo={''} bar={3} />;
//       ^
}

f = () => {
  declare const BaseComp: React.ComponentType<SourceObj>;
  type MapProps<O> = {[K in keyof O]: O[K] extends { +$fragmentType: empty, ... } ? O[K] : O[K]};
  type $RelayProps<T> = MapProps<
    $Diff<T, {relayProp: string | void}>,
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
  type MapType<T> = {+[key in keyof T]: SourceObj[key]};
  declare const Comp: React.ComponentType<MapType<SourceObj>>;
  <Comp foo={''} bar={3} />;
//       ^
}

f = () => {
  type ToSpread = { foo: string };
  declare const Comp: React.ComponentType<{...SourceObj, ...ToSpread}>;
  <Comp foo={''} bar={3} />;
//       ^
  <Comp foo={''} bar={3} />;
//                ^
}

f = () => {
  type A = { foo?: string };
  type B = { foo: string };
  declare const Comp: React.ComponentType<{...A, ...B}>;
  <Comp foo={''} />;
//       ^
}

f = () => {
  type A = { foo: string };
  type B = { foo?: string };
  declare const Comp: React.ComponentType<{...A, ...B}>;
  <Comp foo={''} />;
//       ^
}

f = () => {
  type A = { foo?: string };
  type B = { foo?: string };
  declare const Comp: React.ComponentType<{...A, ...B}>;
  <Comp foo={''} />;
//       ^
}

f = () => {
  declare class Comp extends React.Component<SourceObj> {
    defaultProps: {bar: string};
  }
    <Comp foo={''} bar={3} />;
//         ^
    <Comp foo={''} bar={3} />;
//                  ^
}
