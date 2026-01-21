// interaction.js
// Universal interaction tracking for question types with default values
// (slider, slider_numeric, date, daterange)
//
// These types need special handling because they always have values
// even when not interacted by the user. We use a delayed enable flag
// to ignore initialization and restoration events.

function initInteractionTracking(questionId, questionType) {
  var containerId = 'container-' + questionId;
  var trackingEnabled = false;

  // Enable tracking after a short delay to skip initialization events
  setTimeout(function() {
    trackingEnabled = true;
  }, 500);

  // Track interactions via input/change events on container
  $('#' + containerId).on('input change', function(e) {
    if (trackingEnabled) {
      Shiny.setInputValue(questionId + '_interacted', true, {priority: 'event'});
    }
  });

  // Track direct interactions based on question type
  if (questionType === 'slider' || questionType === 'slider_numeric') {
    // For sliders, track mousedown/touchstart on .irs elements (ionRangeSlider)
    $(document).on('mousedown touchstart', '#' + containerId + ' .irs', function(e) {
      if (trackingEnabled) {
        Shiny.setInputValue(questionId + '_interacted', true, {priority: 'event'});
      }
    });

    // Handle keyboard interaction (arrow keys) for sliders
    $('#' + questionId).on('keydown', function(e) {
      if (trackingEnabled && e.keyCode >= 37 && e.keyCode <= 40) {
        Shiny.setInputValue(questionId + '_interacted', true, {priority: 'event'});
      }
    });

  } else if (questionType === 'date' || questionType === 'daterange') {
    // For date inputs, track mousedown/touchstart on datepicker and input elements
    $(document).on('mousedown touchstart', '#' + containerId + ' .datepicker, #' + containerId + ' input', function(e) {
      if (trackingEnabled) {
        Shiny.setInputValue(questionId + '_interacted', true, {priority: 'event'});
      }
    });
  }
}
