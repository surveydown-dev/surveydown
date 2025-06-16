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
        var questionContainer = $this.closest('.question-container');
        var inputId = $this.attr('id') || $this.attr('name');
        
        // Handle regular questions
        if (questionContainer.hasClass('unanswered-question-highlight')) {
            questionContainer.removeClass('unanswered-question-highlight');
        }
        if (questionContainer.hasClass('required-question-highlight')) {
            questionContainer.removeClass('required-question-highlight');
        }
        
        // Handle matrix questions
        if (inputId && inputId.includes('_')) {
            handleMatrixQuestionHighlighting(inputId);
        }
    });
    
    // Function to handle matrix question highlighting removal
    function handleMatrixQuestionHighlighting(inputId) {
        // Extract potential root matrix question ID (everything before the last underscore)
        var parts = inputId.split('_');
        if (parts.length < 2) return;
        
        // Try different combinations to find the root matrix question
        // Start from the longest possible root ID and work backwards
        for (var i = parts.length - 1; i >= 1; i--) {
            var rootId = parts.slice(0, i).join('_');
            var rootContainer = findQuestionContainer(rootId);
            
            if (rootContainer.length > 0 && rootContainer.find('.matrix-question').length > 0) {
                // This is a matrix question container
                if (isMatrixComplete(rootContainer)) {
                    rootContainer.removeClass('unanswered-question-highlight required-question-highlight');
                }
                break;
            }
        }
    }
    
    // Function to check if a matrix question is completely answered
    function isMatrixComplete(matrixContainer) {
        var allRows = matrixContainer.find('.matrix-question tbody tr');
        var totalRows = allRows.length;
        var answeredRows = 0;
        
        allRows.each(function() {
            var row = $(this);
            var radioButtons = row.find('input[type="radio"]');
            if (radioButtons.length > 0) {
                var hasAnswer = radioButtons.is(':checked');
                if (hasAnswer) {
                    answeredRows++;
                }
            }
        });
        
        return answeredRows === totalRows && totalRows > 0;
    }
    
    // Helper function to find question container (reuse existing logic)
    function findQuestionContainer(questionId) {
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
        
        return questionContainer;
    }
});

// Function to highlight a specific question
function highlightQuestion(questionId, type) {
    type = type || 'required'; // Default to required highlighting
    
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