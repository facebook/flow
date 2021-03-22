// @flow

import type {ClusterSettings, ClusterSettingsOpt} from 'cluster';

const cluster = require('cluster');

(cluster.isMaster: bool);
(cluster.isWorker: bool);
(cluster.settings: ClusterSettings);
(cluster.settings.exec: string); // not optional

// example from node.js cluster.setupMaster docs:
cluster.setupMaster({
  exec: 'worker.js',
  args: ['--use', 'https'],
  silent: true
});
cluster.fork(); // https worker
cluster.setupMaster({
  exec: 'worker.js',
  args: ['--use', 'http']
});
cluster.fork(); // http worker
