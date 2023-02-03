//@flow

function whatsup() {
    type U = {
      customPolygonData: ?{x: number},
    };
    declare var item: U;
    if (item.customPolygonData == null) {
      return null;
    }
    const ret = {
      ...item.customPolygonData,
    };
    (ret: empty); //err
  }
