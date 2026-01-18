// randomization.js
// Handle option randomization for MC questions

$(document).ready(function() {
  // Custom message handler for randomizing question options
  Shiny.addCustomMessageHandler("randomizeQuestionOptions", function(data) {
    if (!data || !data.questions || data.questions.length === 0) {
      return;
    }

    // Use delay if provided, otherwise default to 100ms
    var delay = data.delay || 100;

    setTimeout(function() {
      // Randomize each question
      for (var i = 0; i < data.questions.length; i++) {
        var questionData = data.questions[i];
        randomizeQuestion(questionData.question_id, questionData.order);
      }
    }, delay);
  });
});

// Function to randomize options for a specific question
function randomizeQuestion(questionId, order) {
  if (!order || order.length < 2) {
    // Skip randomization if less than 2 options
    return;
  }

  // Find the question container
  var container = $('[data-question-id="' + questionId + '"]');

  if (container.length === 0) {
    console.warn("Could not find container for question:", questionId);
    return;
  }

  // Determine question type and get option elements
  var optionElements = [];

  // Try radio buttons (mc)
  var radioGroup = container.find('.shiny-input-radiogroup');
  if (radioGroup.length > 0) {
    optionElements = radioGroup.find('> .radio').toArray();
  }

  // Try checkbox group (mc_multiple)
  if (optionElements.length === 0) {
    var checkboxGroup = container.find('.shiny-input-checkboxgroup');
    if (checkboxGroup.length > 0) {
      optionElements = checkboxGroup.find('> .checkbox').toArray();
    }
  }

  // Try button groups (mc_buttons, mc_multiple_buttons)
  if (optionElements.length === 0) {
    var buttonGroup = container.find('.radio-group-buttons, .checkbox-group-buttons');
    if (buttonGroup.length > 0) {
      optionElements = buttonGroup.find('> .btn').toArray();
    }
  }

  if (optionElements.length === 0 || optionElements.length !== order.length) {
    console.warn("Could not randomize options for question:", questionId);
    return;
  }

  // Apply randomization by reordering DOM elements
  var parent = $(optionElements[0]).parent();
  var reorderedElements = [];

  // Create new order based on randomization array (R uses 1-based indexing)
  for (var i = 0; i < order.length; i++) {
    var originalIndex = order[i] - 1; // Convert from 1-based to 0-based
    reorderedElements.push(optionElements[originalIndex]);
  }

  // Detach all elements
  for (var i = 0; i < optionElements.length; i++) {
    $(optionElements[i]).detach();
  }

  // Reattach in randomized order
  for (var i = 0; i < reorderedElements.length; i++) {
    parent.append(reorderedElements[i]);
  }
}
