// Function to save answer to session storage
function saveAnswerToStorage(inputId, value) {
  try {
    let answers = JSON.parse(sessionStorage.getItem('surveyAnswers') || '{}');
    answers[inputId] = value;
    sessionStorage.setItem('surveyAnswers', JSON.stringify(answers));
  } catch (e) {
    console.error('Error saving to session storage:', e);
  }
}

// Function to load answers from session storage
function loadAnswersFromStorage() {
  try {
    const answers = JSON.parse(sessionStorage.getItem('surveyAnswers') || '{}');
    // Send the answers to Shiny
    Shiny.onInputChange('session_storage_answers', answers);
  } catch (e) {
    console.error('Error loading from session storage:', e);
    Shiny.onInputChange('session_storage_answers', {});
  }
}

// Initialize by loading existing answers
$(document).ready(function() {
  loadAnswersFromStorage();
});

// Listen for input changes and save to storage
$(document).on('shiny:inputchanged', function(event) {
  // Only save actual question inputs, not system inputs
  if (!event.name.startsWith('._') && !event.name.includes('session_storage')) {
    saveAnswerToStorage(event.name, event.value);
  }
});

// Clear storage when the survey is completed
function clearSessionStorage() {
  sessionStorage.removeItem('surveyAnswers');
}