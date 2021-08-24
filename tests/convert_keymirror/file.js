// @flow

type Id = <K>(K) => K;

type T1 = $ObjMapi<{ f: '' }, <K>(K) => K>;
type T2 = $ObjMapi<{ f: '' }, <K, V>(K, V) => K>;
type T3 = $ObjMapi<{ f: '' }, <K, V>(K, V) => V>;
type T4 = $ObjMapi<{ f: '' }, Id>;
type T4 = $ObjMapi<{ f: '' }>;
type T5 = $ObjMapi<>;
