$(document).ready(function() {
  window.addEventListener('beforeunload', function(e) {
    Shiny.setInputValue('window_closing', true, {priority: 'event'});
  });
});
