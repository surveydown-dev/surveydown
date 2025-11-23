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
    
    // Custom message handler for highlighting unanswered questions
    Shiny.addCustomMessageHandler("highlightUnansweredQuestions", function(data) {
        // Debug logging
        console.log("Highlighting unanswered questions:", data.questions, "color:", data.color);
        
        // Handle different data formats that R might send
        var questions = [];
        var color = data.color || 'gray'; // Default to gray if no color specified
        
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
            // Highlight each unanswered question with the specified color
            for (var i = 0; i < questions.length; i++) {
                var questionId = questions[i];
                highlightQuestion(questionId, 'unanswered', color);
            }
        }
    });
    
    // Custom message handler for highlighting validation questions  
    Shiny.addCustomMessageHandler("highlightValidationQuestions", function(data) {
        // Clear any existing highlights first
        clearValidationHighlights();
        
        // Debug logging (can be removed in production)
        console.log("Highlighting validation questions:", data.questions);
        
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
            // Highlight each validation failed question in red
            for (var i = 0; i < questions.length; i++) {
                var questionId = questions[i];
                highlightQuestion(questionId, 'validation');
            }
            
            // Scroll to the first highlighted question
            scrollToFirstHighlighted(questions[0]);
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
    
    // Custom message handler for clearing validation highlights
    Shiny.addCustomMessageHandler("clearValidationHighlights", function(data) {
        clearValidationHighlights();
    });
    
    // Custom message handler for delayed highlighting check
    Shiny.addCustomMessageHandler("delayedHighlightCheck", function(data) {
        setTimeout(function() {
            // Trigger server-side highlighting check
            Shiny.setInputValue("delayed_highlight_trigger", Date.now(), {priority: "event"});
        }, data.delay);
    });

    // Clear highlights when user starts answering questions
    $(document).on('change input', '.question-container input, .question-container select, .question-container textarea', function() {
        var $this = $(this);
        var inputName = $this.attr('name') || $this.attr('id');
        
        // Always try to remove highlighting from question container first
        var questionContainer = $this.closest('.question-container');
        if (questionContainer.length > 0) {
            questionContainer.removeClass('unanswered-question-highlight unanswered-question-highlight-orange unanswered-question-highlight-green unanswered-question-highlight-purple unanswered-question-highlight-gray required-question-highlight validation-question-highlight');
        }
        
        // For radio buttons, also check if this is a matrix subquestion and handle the radio group
        if ($this.is('input[type="radio"]') && inputName && inputName.includes('_')) {
            // This might be a matrix subquestion - remove highlighting from the radio group too
            var radioGroup = $this.closest('.shiny-input-radiogroup');
            if (radioGroup.length > 0) {
                radioGroup.removeClass('unanswered-question-highlight unanswered-question-highlight-orange unanswered-question-highlight-green unanswered-question-highlight-purple unanswered-question-highlight-gray required-question-highlight validation-question-highlight');
                radioGroup.find('input[type="radio"]').removeClass('unanswered-question-highlight unanswered-question-highlight-orange unanswered-question-highlight-green unanswered-question-highlight-purple unanswered-question-highlight-gray required-question-highlight validation-question-highlight');
            }
        }
    });
});

// Function to highlight a specific question
function highlightQuestion(questionId, type, color) {
    type = type || 'required'; // Default to required highlighting
    color = color || 'gray'; // Default to gray for unanswered questions
    
    // Check if this is a matrix subquestion (contains underscore)
    if (questionId.includes('_')) {
        var targetElement = highlightMatrixSubquestion(questionId, type, color);
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
        var highlightClass;
        
        if (type === 'required') {
            highlightClass = 'required-question-highlight';
            // Priority system: remove any existing unanswered highlighting
            questionContainer.removeClass('unanswered-question-highlight unanswered-question-highlight-orange unanswered-question-highlight-green unanswered-question-highlight-purple unanswered-question-highlight-gray validation-question-highlight');
        } else if (type === 'validation') {
            highlightClass = 'validation-question-highlight';
            // Priority system: remove any existing highlights
            questionContainer.removeClass('required-question-highlight unanswered-question-highlight unanswered-question-highlight-orange unanswered-question-highlight-green unanswered-question-highlight-purple unanswered-question-highlight-gray');
        } else {
            // For unanswered questions, use color-specific class
            // Handle both gray and grey spellings
            if (color === 'grey') color = 'gray';
            highlightClass = color === 'blue' ? 'unanswered-question-highlight' : 'unanswered-question-highlight-' + color;
        }
        
        questionContainer.addClass(highlightClass);
        
        // Also highlight any form controls inside
        questionContainer.find('.form-control, input, select, textarea').addClass(highlightClass);
    }
}

// Function to highlight a specific matrix subquestion
function highlightMatrixSubquestion(subquestionId, type, color) {
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
        var highlightClass;
        
        if (type === 'required') {
            highlightClass = 'required-question-highlight';
            // Priority system: remove any existing unanswered highlighting
            radioGroup.removeClass('unanswered-question-highlight unanswered-question-highlight-orange unanswered-question-highlight-green unanswered-question-highlight-purple unanswered-question-highlight-gray validation-question-highlight');
        } else if (type === 'validation') {
            highlightClass = 'validation-question-highlight';
            // Priority system: remove any existing highlights
            radioGroup.removeClass('required-question-highlight unanswered-question-highlight unanswered-question-highlight-orange unanswered-question-highlight-green unanswered-question-highlight-purple unanswered-question-highlight-gray');
        } else {
            // For unanswered questions, use color-specific class
            color = color || 'gray';
            // Handle both gray and grey spellings
            if (color === 'grey') color = 'gray';
            highlightClass = color === 'blue' ? 'unanswered-question-highlight' : 'unanswered-question-highlight-' + color;
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
    $('.unanswered-question-highlight, .unanswered-question-highlight-orange, .unanswered-question-highlight-green, .unanswered-question-highlight-purple, .unanswered-question-highlight-gray')
        .removeClass('unanswered-question-highlight unanswered-question-highlight-orange unanswered-question-highlight-green unanswered-question-highlight-purple unanswered-question-highlight-gray');
}

// Function to clear all validation question highlights
function clearValidationHighlights() {
    $('.validation-question-highlight').removeClass('validation-question-highlight');
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