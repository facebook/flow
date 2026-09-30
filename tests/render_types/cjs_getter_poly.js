import typeof {Poly as PolyT} from './poly_no_args';

module.exports = {
  // $FlowExpectedError[unsafe-getters-setters]
  get Poly(): PolyT {
    return require('./poly_no_args').Poly;
  },
};
