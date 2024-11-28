$(document).ready(function() {
  window.addEventListener('beforeunload', function(e) {
    // Send message to Shiny
    Shiny.setInputValue('window_closing', true, {priority: 'event'});

    // Small delay to allow data to save
    var start = Date.now();
    while(Date.now() - start < 200) {}
  });
});
