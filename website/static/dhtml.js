function highlightErrors(errors) {
  var errorMessage = $('<div class="errmsg"></div>');
  $(document.body).append(errorMessage);

  $('.error').mouseenter(function() {
    var eID = $(this).attr('data-error-id');
    var mID = $(this).attr('data-message-id');

    errorMessage.css({top: $(this).offset().top});
    $('.selected').removeClass('selected');
    $('.main-selected').removeClass('main-selected');

    $('[data-error-id=' + eID +']').addClass('selected');
    $(this).addClass('main-selected');

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
  })
  .mouseleave(function() {
    $('.selected').removeClass('selected');
    $('.main-selected').removeClass('main-selected');
    errorMessage.empty();
  });
}
