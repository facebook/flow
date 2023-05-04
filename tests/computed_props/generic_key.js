type Map = $ReadOnly<{[string]: string}>;

function Component<TMap: Map>() {
  declare var map: TMap;

  function addToMap<TKey: $Keys<TMap>>(key: TKey, value: TMap[TKey]): TMap {
    return {
      ...map,
      [key]: value, // okay
    };
  }

  type KeysAlias<TMap: Map> = $Keys<TMap>;
  function addToMap2<TKey: KeysAlias<TMap>>(
    key: TKey,
    value: TMap[TKey],
  ): TMap {
    return {
      ...map,
      [key]: value, // okay
    };
  }
}
