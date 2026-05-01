const Immutable = require('immutable');

const tasksPerStatusMap = new MyMap(
  ([] as Array<string>).map(taskStatus => [taskStatus, new MyMap() as MyMap<string, string> | Immutable.Map<string, string>]),
);
for (let [taskStatus, tasksMap] of tasksPerStatusMap) {
  tasksPerStatusMap.set(taskStatus, Immutable.Map(tasksMap));
}
