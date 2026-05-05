type Map = Readonly<{[string]: string}>;

function Component<TMap extends Map>() {
  declare var map: TMap;

  function addToMap<TKey extends keyof TMap>(key: TKey, value: TMap[TKey]): TMap {
    return {
      ...map,
      [key]: value, // error: overwriting
    };
  }

  type KeysAlias<TMap extends Map> = keyof TMap;
  function addToMap2<TKey extends KeysAlias<TMap>>(
    key: TKey,
    value: TMap[TKey],
  ): TMap {
    return {
      ...map,
      [key]: value, // error: overwriting
    };
  }
}
