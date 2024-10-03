// Global configuration object
window.surveydownConfig = {
  autoScrollEnabled: true  // Default value, will be set by R
};

// Function to scroll to the top of the page
function scrollToPageTop() {
  window.scrollTo({
    top: 0,
    behavior: 'smooth'
  });
}

// Function to auto-scroll to the next question
function autoScrollToNextQuestion(currentQuestionId) {
  if (!window.surveydownConfig.autoScrollEnabled) return;

  const currentContainer = document.getElementById(`container-${currentQuestionId}`);
  if (!currentContainer) return;

  const allQuestions = document.querySelectorAll('.question-container');
  let nextQuestion = null;

  for (let i = 0; i < allQuestions.length; i++) {
    if (allQuestions[i] === currentContainer) {
      if (i < allQuestions.length - 1) {
        nextQuestion = allQuestions[i + 1];
        break;
      }
    }
  }

  if (nextQuestion) {
    scrollBehavior(nextQuestion, 400); // 400ms = 0.4 seconds
  }
}

function scrollBehavior(element, duration) {
  const start = window.pageYOffset;
  const elementRect = element.getBoundingClientRect();
  const elementTop = elementRect.top + start;
  const centerOffset = window.innerHeight / 2 - elementRect.height / 2;
  const target = elementTop - centerOffset;
  const startTime = performance.now();

  function animate(currentTime) {
    const elapsedTime = currentTime - startTime;
    const progress = Math.min(elapsedTime / duration, 1);
    window.scrollTo(0, start + (target - start) * easeInOutQuad(progress));
    if (progress < 1) {
      requestAnimationFrame(animate);
    }
  }

  function easeInOutQuad(t) {
    return t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t;
  }

  requestAnimationFrame(animate);
}

// Function to handle page changes
function handlePageChange() {
  scrollToPageTop();
}

// Modify the existing question interaction logic
$(document).on('shiny:inputchanged', function(event) {
  if (event.name.endsWith('_interacted')) {
    const questionId = event.name.replace('_interacted', '');
    setTimeout(() => {
      autoScrollToNextQuestion(questionId);
    }, 400); // 400ms = 0.4 second delay
  }
});

// Listen for page changes
$(document).on('shiny:value', function(event) {
  if (event.name === 'main') {
    // Use setTimeout to ensure the new page content is rendered
    setTimeout(handlePageChange, 100);
  }
});

// Function to update the configuration
function updateSurveydownConfig(config) {
  window.surveydownConfig = {...window.surveydownConfig, ...config};
}

// Add Shiny message handler
Shiny.addCustomMessageHandler("updateSurveydownConfig", function(message) {
  updateSurveydownConfig(message);
});
