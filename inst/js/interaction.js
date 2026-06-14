// interaction.js
// Universal interaction tracking for question types with default values
// (slider, slider_numeric, date, daterange), plus delegated handlers for
// button-group and numeric questions.
//
// The delegated handlers are registered ONCE on the document and work for
// every current and future question of that type, including reactive
// questions that re-render. (Previously each question carried its own
// inline script, which re-registered handlers on every re-render.)

// ---------------------------------------------------------------------------
// Delegated handlers for mc_buttons / mc_multiple_buttons / numeric
// ---------------------------------------------------------------------------

$(document).ready(function() {
  // mc_buttons: report the selected value after a button click
  $(document).on('click', '.question-container .radio-group-buttons .btn', function() {
    var widget = $(this).closest('.radio-group-buttons');
    var questionId = widget.attr('id');
    if (!questionId) return;
    Shiny.setInputValue(questionId + '_interacted', true, {priority: 'event'});
    // Small delay to allow the button state to update before reading it
    setTimeout(function() {
      var checkedInput = widget.find('input[type="radio"]:checked');
      var selectedValue = checkedInput.length > 0 ? checkedInput.val() : '';
      Shiny.setInputValue(questionId, selectedValue, {priority: 'event'});
    }, 50);
  });

  // mc_multiple_buttons: report all selected values after a button click
  $(document).on('click', '.question-container .checkbox-group-buttons .btn', function() {
    var widget = $(this).closest('.checkbox-group-buttons');
    var questionId = widget.attr('id');
    if (!questionId) return;
    Shiny.setInputValue(questionId + '_interacted', true, {priority: 'event'});
    setTimeout(function() {
      var selectedValues = [];
      widget.find('input[type="checkbox"]:checked').each(function() {
        selectedValues.push($(this).val());
      });
      Shiny.setInputValue(questionId, selectedValues, {priority: 'event'});
    }, 50);
  });

  // numeric: interaction tracking on keyboard focus (the question
  // container's oninput/onclick attributes cover typing and clicking)
  $(document).on('focus', 'input.sd-numeric', function() {
    Shiny.setInputValue(this.id + '_interacted', true, {priority: 'event'});
  });

  // numeric: validation - digits, one leading +/- sign, one decimal point
  $(document).on('input', 'input.sd-numeric', function() {
    var val = $(this).val();
    var filtered = '';
    var hasDecimal = false;
    var hasSign = false;
    for (var i = 0; i < val.length; i++) {
      var char = val[i];
      // Allow +/- only at the beginning and only one
      if ((char === '+' || char === '-') && i === 0 && !hasSign) {
        filtered += char;
        hasSign = true;
      } else if (/[0-9]/.test(char)) {
        filtered += char;
      } else if (char === '.' && !hasDecimal) {
        filtered += char;
        hasDecimal = true;
      }
    }
    if (val !== filtered) {
      $(this).val(filtered);
    }
  });

  // numeric: re-validate after paste
  $(document).on('paste', 'input.sd-numeric', function() {
    var el = $(this);
    setTimeout(function() { el.trigger('input'); }, 1);
  });

  // numeric: spinner button clicks (buttons are injected on bind below)
  $(document).on('mousedown', '.native-spinner-button', function(e) {
    e.preventDefault();
    var inputElement = $(this).closest('.numeric-input-container').find('input.sd-numeric');
    var currentVal = parseFloat(inputElement.val()) || 0;
    var newVal = $(this).hasClass('spinner-up') ? currentVal + 1 : currentVal - 1;
    inputElement.val(newVal).trigger('input');
  });
});

// Wrap each numeric input with the native-style spinner when Shiny binds
// it (fires on every page render and reactive question re-render)
$(document).on('shiny:bound', function(e) {
  var el = e.target;
  if (!el || !el.classList || !el.classList.contains('sd-numeric')) return;
  if (el.dataset.sdSpinnerInit) return;
  el.dataset.sdSpinnerInit = '1';
  var inputElement = $(el);
  inputElement.addClass('numeric-input-with-spinner');
  inputElement.wrap('<div class="numeric-input-container"></div>');
  inputElement.after(
    '<div class="native-spinner">' +
      '<button type="button" class="native-spinner-button spinner-up" tabindex="-1"></button>' +
      '<button type="button" class="native-spinner-button spinner-down" tabindex="-1"></button>' +
    '</div>'
  );
});

// ---------------------------------------------------------------------------
// Interaction tracking for types with default values
// ---------------------------------------------------------------------------
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
