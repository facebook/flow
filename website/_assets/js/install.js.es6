/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as $ from 'jquery';

var toggles = $('.install-toggle');
var inputs = toggles.find('input');
var buttons = toggles.find('.btn');
var guides = $('.install-guide');

function update() {
  var match = {};

  inputs.each(function(index, input) {
    if (input.checked) {
      match[input.name] = input.value;
    }
    // bootstrap does this for us, but it's one of the only things it does so
    // we could do this instead of pulling in all of bootstrap.js (the other
    // thing it handles is the mobile hamburger right now)
    // $(input).parent('.btn').toggleClass('active', input.checked);
  });

  guides.each(function(index, guide) {
    let matched = Object.keys(match).every(function(key) {
      return guide.dataset[key] === match[key];
    });

    var $guide = $(guide);

    $guide.toggleClass('active', matched);
    $guide.attr('aria-hidden', !matched);
  });
}

update();
buttons.on('change', update);
inputs.on('change', update);
