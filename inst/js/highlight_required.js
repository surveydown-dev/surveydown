// Handle highlighting of required questions
$(document).ready(function() {
    // Custom message handler for highlighting required questions
    Shiny.addCustomMessageHandler("highlightRequiredQuestions", function(data) {
        // Clear any existing highlights
        clearRequiredHighlights();
        
        // Debug logging (can be removed in production)
        console.log("Highlighting required questions:", data.questions);
        
        // Handle different data formats that R might send
        var questions = [];
        
        if (data && data.questions) {
            if (Array.isArray(data.questions)) {
                questions = data.questions;
            } else if (typeof data.questions === 'string') {
                questions = [data.questions];
            } else if (typeof data.questions === 'object') {
                // Handle R list/vector converted to object
                try {
                    questions = Object.values(data.questions);
                } catch (e) {
                    console.error("Error converting questions object to array:", e);
                    questions = [];
                }
            }
        }
        
        // Ensure all elements are strings
        questions = questions.filter(function(q) {
            return q && typeof q === 'string' && q.length > 0;
        });
        
        if (questions && questions.length > 0) {
            // Highlight each unanswered required question
            for (var i = 0; i < questions.length; i++) {
                var questionId = questions[i];
                highlightQuestion(questionId, 'required');
            }
            
            // Scroll to the first highlighted question
            scrollToFirstHighlighted(questions[0]);
        }
    });
    
    // Custom message handler for highlighting unanswered questions (blue)
    Shiny.addCustomMessageHandler("highlightUnansweredQuestions", function(data) {
        // Debug logging
        console.log("Highlighting unanswered questions:", data.questions);
        
        // Handle different data formats that R might send
        var questions = [];
        
        if (data && data.questions) {
            if (Array.isArray(data.questions)) {
                questions = data.questions;
            } else if (typeof data.questions === 'string') {
                questions = [data.questions];
            } else if (typeof data.questions === 'object') {
                try {
                    questions = Object.values(data.questions);
                } catch (e) {
                    console.error("Error converting questions object to array:", e);
                    questions = [];
                }
            }
        }
        
        // Ensure all elements are strings
        questions = questions.filter(function(q) {
            return q && typeof q === 'string' && q.length > 0;
        });
        
        if (questions && questions.length > 0) {
            // Highlight each unanswered question
            for (var i = 0; i < questions.length; i++) {
                var questionId = questions[i];
                highlightQuestion(questionId, 'unanswered');
            }
        }
    });
    
    // Custom message handler for clearing highlights
    Shiny.addCustomMessageHandler("clearRequiredHighlights", function(data) {
        clearRequiredHighlights();
    });
    
    // Custom message handler for clearing unanswered highlights
    Shiny.addCustomMessageHandler("clearUnansweredHighlights", function(data) {
        clearUnansweredHighlights();
    });
    
    // Clear highlights when user starts answering questions
    $(document).on('change input', '.question-container input, .question-container select, .question-container textarea', function() {
        var $this = $(this);
        var inputName = $this.attr('name') || $this.attr('id');
        
        // Always try to remove highlighting from question container first
        var questionContainer = $this.closest('.question-container');
        if (questionContainer.length > 0) {
            questionContainer.removeClass('unanswered-question-highlight required-question-highlight');
        }
        
        // For radio buttons, also check if this is a matrix subquestion and handle the radio group
        if ($this.is('input[type="radio"]') && inputName && inputName.includes('_')) {
            // This might be a matrix subquestion - remove highlighting from the radio group too
            var radioGroup = $this.closest('.shiny-input-radiogroup');
            if (radioGroup.length > 0) {
                radioGroup.removeClass('unanswered-question-highlight required-question-highlight');
                radioGroup.find('input[type="radio"]').removeClass('unanswered-question-highlight required-question-highlight');
            }
        }
    });
});

// Function to highlight a specific question
function highlightQuestion(questionId, type) {
    type = type || 'required'; // Default to required highlighting
    
    // Check if this is a matrix subquestion (contains underscore)
    if (questionId.includes('_')) {
        var targetElement = highlightMatrixSubquestion(questionId, type);
        if (targetElement && targetElement.length > 0) {
            return; // Successfully highlighted matrix subquestion
        }
    }
    
    // Try multiple strategies to find the question container for regular questions
    var questionContainer = $();
    
    // Strategy 1: Look for data-question-id attribute
    questionContainer = $('[data-question-id="' + questionId + '"]');
    
    // Strategy 2: Look for container-{id} pattern
    if (questionContainer.length === 0) {
        questionContainer = $('#container-' + questionId);
    }
    
    // Strategy 3: Find input element and traverse up
    if (questionContainer.length === 0) {
        var input = $('#' + questionId);
        if (input.length > 0) {
            questionContainer = input.closest('.question-container, .form-group, .shiny-input-container');
        }
    }
    
    // Strategy 4: Look for any element with the questionId and find its container
    if (questionContainer.length === 0) {
        var element = $('[id="' + questionId + '"], [name="' + questionId + '"]');
        if (element.length > 0) {
            questionContainer = element.closest('.question-container, .form-group, .shiny-input-container');
        }
    }
    
    if (questionContainer.length > 0) {
        var highlightClass = type === 'required' ? 'required-question-highlight' : 'unanswered-question-highlight';
        
        // Priority system: if this is a required question highlighting, remove any existing blue highlighting
        if (type === 'required') {
            questionContainer.removeClass('unanswered-question-highlight');
        }
        
        questionContainer.addClass(highlightClass);
        
        // Also highlight any form controls inside
        questionContainer.find('.form-control, input, select, textarea').addClass(highlightClass);
    }
}

// Function to highlight a specific matrix subquestion
function highlightMatrixSubquestion(subquestionId, type) {
    console.log("Highlighting matrix subquestion:", subquestionId, "type:", type);
    
    // Find the specific radio group for this subquestion
    var radioGroup = $('[name="' + subquestionId + '"]').closest('.shiny-input-radiogroup');
    
    if (radioGroup.length === 0) {
        // Try alternative approach - look for the input directly
        var input = $('#' + subquestionId);
        if (input.length > 0) {
            radioGroup = input.closest('.shiny-input-radiogroup');
        }
    }
    
    console.log("Found radio group for", subquestionId, ":", radioGroup.length > 0);
    
    if (radioGroup.length > 0) {
        var highlightClass = type === 'required' ? 'required-question-highlight' : 'unanswered-question-highlight';
        
        // Priority system: if this is a required question highlighting, remove any existing blue highlighting
        if (type === 'required') {
            radioGroup.removeClass('unanswered-question-highlight');
        }
        
        radioGroup.addClass(highlightClass);
        
        // Also highlight the radio buttons inside
        radioGroup.find('input[type="radio"]').addClass(highlightClass);
        
        return radioGroup;
    }
    
    return null;
}

// Function to clear all required question highlights
function clearRequiredHighlights() {
    $('.required-question-highlight').removeClass('required-question-highlight');
}

// Function to clear all unanswered question highlights
function clearUnansweredHighlights() {
    $('.unanswered-question-highlight').removeClass('unanswered-question-highlight');
}

// Function to scroll to the first highlighted question
function scrollToFirstHighlighted(questionId) {
    var questionContainer = $('[data-question-id="' + questionId + '"]');
    
    if (questionContainer.length === 0) {
        questionContainer = $('#container-' + questionId);
    }
    
    if (questionContainer.length === 0) {
        var input = $('#' + questionId);
        if (input.length > 0) {
            questionContainer = input.closest('.question-container, .form-group');
        }
    }
    
    if (questionContainer.length > 0) {
        // Smooth scroll to the question
        $('html, body').animate({
            scrollTop: questionContainer.offset().top - 100
        }, 500);
    }
}