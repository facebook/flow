//@flow

function whatsup() {
  type U = {
    customPolygonData: ?{x: number, ...},
 ...  };
  declare const item: U;
  if (item.customPolygonData == null) {
    return null;
  }
  const ret = {
    ...item.customPolygonData,
  };
  ret as empty; //err
}
