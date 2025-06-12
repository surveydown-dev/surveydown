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
                highlightQuestion(questionId);
            }
            
            // Scroll to the first highlighted question
            scrollToFirstHighlighted(questions[0]);
        }
    });
    
    // Custom message handler for clearing highlights
    Shiny.addCustomMessageHandler("clearRequiredHighlights", function(data) {
        clearRequiredHighlights();
    });
    
    // Clear highlights when user starts answering questions
    $(document).on('change input', '.question-container input, .question-container select, .question-container textarea', function() {
        var questionContainer = $(this).closest('.question-container');
        if (questionContainer.hasClass('required-question-highlight')) {
            questionContainer.removeClass('required-question-highlight');
        }
    });
});

// Function to highlight a specific question
function highlightQuestion(questionId) {
    // Try multiple strategies to find the question container
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
        questionContainer.addClass('required-question-highlight');
        
        // Also highlight any form controls inside
        questionContainer.find('.form-control, input, select, textarea').addClass('required-question-highlight');
    }
}

// Function to clear all required question highlights
function clearRequiredHighlights() {
    $('.required-question-highlight').removeClass('required-question-highlight');
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