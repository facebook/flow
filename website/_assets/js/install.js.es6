import * as $ from 'jquery';

var toggles = $('.install-toggle');
var inputs = toggles.find('input');
var buttons = toggles.find('.btn');
var guides = $('.install-guide');

function update() {
  var match = {};

  inputs.each(function(index, input) {
    if (input.checked) match[input.name] = input.value;
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
