type Transform = {
  id: string;
  rotation?: number[];
  scale?: number[];
};

function buildTransform(item: Transform): Transform {
  return {
    id: item.id,
    ...(item.rotation ? { rotation: [...item.rotation] } : {}),
    ...(item.scale ? { scale: [...item.scale] } : {}),
  };
}

console.log(buildTransform({ id: "node", rotation: [0, 90, 0] }));
