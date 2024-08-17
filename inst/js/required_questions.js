window.initializeRequiredQuestions = function(requiredQuestions) {
    console.log("initializeRequiredQuestions called with:", requiredQuestions);
    if (!Array.isArray(requiredQuestions) || requiredQuestions.length === 0) {
        console.warn("No required questions provided or invalid input");
        return;
    }

    function showAsterisk(id) {
        var container = document.querySelector(`[data-question-id="${id}"]`) ||
                        document.getElementById(`container-${id}`) ||
                        document.querySelector(`.question-container[id$="-${id}"]`);

        if (!container) {
            console.warn(`Container not found for question ID: ${id}`);
            return;
        }

        var asterisk = container.querySelector('.required-asterisk') ||
                       container.querySelector('span[style*="display:none"]');

        if (!asterisk) {
            console.warn(`Asterisk not found for question ID: ${id}`);
            return;
        }

        asterisk.style.display = 'inline';
        console.log(`Asterisk display set to inline for question: ${id}`);
    }

    // Initial check for all required questions
    requiredQuestions.forEach(showAsterisk);

    // Set up a MutationObserver to watch for changes
    var observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
            if (mutation.type === 'childList') {
                requiredQuestions.forEach(showAsterisk);
            }
        });
    });

    // Start observing the document with the configured parameters
    observer.observe(document.body, { childList: true, subtree: true });
};

document.addEventListener('DOMContentLoaded', function() {
    console.log("DOM fully loaded and parsed");
});

// Function to check if Shiny is initialized
function checkShinyReady(callback) {
    if (window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected()) {
        callback();
    } else {
        setTimeout(function() { checkShinyReady(callback); }, 100);
    }
}

checkShinyReady(function() {
    console.log("Shiny is ready");
});

console.log("required_questions.js loaded");
