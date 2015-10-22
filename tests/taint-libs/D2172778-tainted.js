// @flow

var fbLawEnforcementRetentionDialog = {

  init: function(dialog : any, download_uri : $Tainted<Location>) {
    dialog.subscribe('confirm', function() {
      dialog.hide();
      document.location = download_uri;
    });
  }

};
