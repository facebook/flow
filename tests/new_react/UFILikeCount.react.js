/**
 * Copyright 2004-present Facebook. All Rights Reserved.
 *
 * @providesModule UFILikeCount.react
 * @flow
 */

'use strict';

var React = require('react');
import type {Node} from 'react';

var UFILikeCount = React.createClass({
  propTypes: {
    permalink: React.PropTypes.string,
    feedback: React.PropTypes.object.isRequired
  },

  render: function(): Node {
    return <div/>;
  }
});

module.exports = UFILikeCount;
