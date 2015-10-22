// @flow

var fbLawEnforcementRetentionDialog = {

  init: function(dialog : any, download_uri : $Tainted<Location>) {
    dialog.subscribe('confirm', function() {
      dialog.hide();
      goURI(download_uri);
    });
  }

};
