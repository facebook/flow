type Map = Readonly<{[string]: string}>;

function Component<TMap: Map>() {
  declare var map: TMap;

  function addToMap<TKey: keyof TMap>(key: TKey, value: TMap[TKey]): TMap {
    return {
      ...map,
      [key]: value, // error: overwriting
    };
  }

  type KeysAlias<TMap: Map> = keyof TMap;
  function addToMap2<TKey: KeysAlias<TMap>>(
    key: TKey,
    value: TMap[TKey],
  ): TMap {
    return {
      ...map,
      [key]: value, // error: overwriting
    };
  }
}
