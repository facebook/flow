/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * @providesModule FeedUFI.react
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

var FeedUFI = React.createClass({
  _renderLikeCount: function(
      feedback: any
  ) {
    var props = {
      className: "",
      key: "",
      feedback: {feedback},
      permalink: "",
    };
    var ignored = <UFILikeCount {...props} />;
    return (
      <UFILikeCount
        className=""
        key=""
        feedback={feedback}
        permalink=""
      />
    );
  },

  render: function(): Node {
    return (
      <div/>
    );
  }

});
