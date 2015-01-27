/**
 * Copyright 2004-present Facebook. All Rights Reserved.
 *
 * @providesModule UFILikeCount.react
 * @flow
 */

'use strict';

var React = require('React');

var UFILikeCount = React.createClass({
  propTypes: {
    permalink: React.PropTypes.string,
    feedback: React.PropTypes.object.isRequired
  },

  render: function(): ?ReactElement<any, any, any> {
    return <div/>;
  }
});

module.exports = UFILikeCount;
