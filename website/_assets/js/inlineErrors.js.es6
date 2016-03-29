//= require jquery/dist/jquery.slim.min.js

import * as $ from "jquery"

function highlightErrors(errors) {
  var errorMessage = $('<div class="errmsg"></div>');
  errorMessage.hide();

  var timeout;
  var hoverStack = [];

  var onenter = function(elem) {
    clearTimeout(timeout);
    var offset = elem.offset();
    var eID = elem.attr('data-error-id');
    var mID = elem.attr('data-message-id');

    errorMessage.css({top: offset.top + elem.height() + 5, left: offset.left});
    $('.selected').removeClass('selected');
    $('.main-selected').removeClass('main-selected');

    $('[data-error-id=' + eID +']').addClass('selected');
    elem.addClass('main-selected');

    errors.forEach(function(error) {
      if (error.id === eID) {
        errorMessage.empty();
        error.messages.forEach(function(message) {
          errorMessage.append(' ');
          var element = $('<span></span>').text(message.description);
          if (message.start && message.end) {
            element.addClass('selected');
          }
          if (message.id === mID) {
            element.addClass('main-selected');
          }
          errorMessage.append(element);
        });
      }
    });
    errorMessage.show();
  }

  $(document.body).append(errorMessage);

  $('.error')
  .mouseover(function() {
    var elem = $(this);
    hoverStack.push(elem);
    onenter(hoverStack[0]);
  })
  .mouseout(function() {
    hoverStack.pop();
    if (hoverStack.length > 0) {
      onenter(hoverStack[0]);
    } else {
      timeout = setTimeout(function() {
        $('.selected').removeClass('selected');
        $('.main-selected').removeClass('main-selected');
        errorMessage.empty();
        errorMessage.hide()
      }, 100);
    }
  });
}

exports.highlight = highlightErrors;
