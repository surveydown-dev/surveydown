const surveydownCookies = {
    set: function(sessionId) {
        try {
            const date = new Date();
            date.setTime(date.getTime() + (30 * 24 * 60 * 60 * 1000)); // 30 days
            const cookieValue = "surveydown_session=" + sessionId +
                              ";expires=" + date.toUTCString() +
                              ";path=/;SameSite=Strict";
            document.cookie = cookieValue;
        } catch (e) {
            console.error("Error setting session cookie:", e);
        }
    },

    get: function() {
        try {
            const name = "surveydown_session=";
            const decodedCookie = decodeURIComponent(document.cookie);
            const ca = decodedCookie.split(';');
            for(let i = 0; i < ca.length; i++) {
                let c = ca[i];
                while (c.charAt(0) == ' ') {
                    c = c.substring(1);
                }
                if (c.indexOf(name) == 0) {
                    return c.substring(name.length);
                }
            }
            return null;
        } catch (e) {
            console.error("Error getting session cookie:", e);
            return null;
        }
    },

    setAnswerData: function(pageId, pageData) {
        try {
            // Only store current page data instead of accumulating
            let currentData = {};
            currentData[pageId] = {
                answers: pageData.answers,
                last_timestamp: pageData.last_timestamp
            };

            // Store page_history separately at top level (not per-page)
            if (pageData.page_history) {
                currentData.page_history = pageData.page_history;
            }

            // Store question_history separately at top level (not per-page)
            if (pageData.question_history) {
                currentData.question_history = pageData.question_history;
            }

            const date = new Date();
            date.setTime(date.getTime() + (30 * 24 * 60 * 60 * 1000));
            const cookieValue = "surveydown_answers=" + JSON.stringify(currentData) +
                              ";expires=" + date.toUTCString() +
                              ";path=/;SameSite=Strict";
            document.cookie = cookieValue;

            // Update Shiny input
            Shiny.setInputValue('stored_answer_data', currentData, {priority: 'event'});
        } catch (e) {
            console.error("Error setting answer data:", e);
        }
    },

    getAnswerData: function() {
        try {
            const name = "surveydown_answers=";
            const decodedCookie = decodeURIComponent(document.cookie);
            const ca = decodedCookie.split(';');
            for(let i = 0; i < ca.length; i++) {
                let c = ca[i];
                while (c.charAt(0) == ' ') {
                    c = c.substring(1);
                }
                if (c.indexOf(name) == 0) {
                    const data = JSON.parse(c.substring(name.length));
                    return data;
                }
            }
            return null;
        } catch (e) {
            console.error("Error getting answer data:", e);
            return null;
        }
    },

    clear: function() {
        try {
            // Clear survey session cookie
            document.cookie = "surveydown_session=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/; SameSite=Strict";
            
            // Clear survey answers cookie  
            document.cookie = "surveydown_answers=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/; SameSite=Strict";
            
            console.log("Survey cookies cleared");
        } catch (e) {
            console.error("Error clearing survey cookies:", e);
        }
    },

    forceRestart: function() {
        try {
            // Clear all survey cookies
            this.clear();
            
            // Log restart action
            console.log("Survey restart initiated");
            
            // Note: Page reload will be handled by the calling function
        } catch (e) {
            console.error("Error during force restart:", e);
        }
    }
};

Shiny.addCustomMessageHandler('setCookie', function(message) {
    if (message.sessionId) {
        surveydownCookies.set(message.sessionId);
    }
});

Shiny.addCustomMessageHandler('setAnswerData', function(message) {
    if (message.pageId && message.pageData) {
        surveydownCookies.setAnswerData(message.pageId, message.pageData);
    }
});

Shiny.addCustomMessageHandler('updatePageHistory', function(message) {
    if (message.pageId && message.pageData && message.pageData.page_history) {
        // Get existing cookie data
        const existingData = surveydownCookies.getAnswerData() || {};
        const existingPageData = existingData[message.pageId] || {};

        // Merge: preserve existing answers but update page_history and question_history at top level
        const mergedPageData = {
            answers: existingPageData.answers || {},
            last_timestamp: existingPageData.last_timestamp || null,
            page_history: message.pageData.page_history,  // Will be stored at top level
            question_history: message.pageData.question_history || []  // Will be stored at top level
        };

        // Update cookie with merged data
        surveydownCookies.setAnswerData(message.pageId, mergedPageData);
    }
});

// Initialize on document ready
$(document).ready(function() {
    function initializeSession(retryCount = 0) {
        const sessionId = surveydownCookies.get();
        const answerData = surveydownCookies.getAnswerData();

        if (sessionId) {
            Shiny.setInputValue('stored_session_id', sessionId, {priority: 'event'});
        }
        if (answerData) {
            Shiny.setInputValue('stored_answer_data', answerData, {priority: 'event'});
        }

        if (!sessionId && retryCount < 3) {
            setTimeout(() => initializeSession(retryCount + 1), 100);
        }
    }

    initializeSession();
});

// Handle Shiny reconnections
$(document).on('shiny:connected', function(event) {
    const sessionId = surveydownCookies.get();
    const answerData = surveydownCookies.getAnswerData();

    if (sessionId) {
        Shiny.setInputValue('stored_session_id', sessionId, {priority: 'event'});
    }
    if (answerData) {
        Shiny.setInputValue('stored_answer_data', answerData, {priority: 'event'});
    }
});

// Handle restoration of slider inputs (text sliders using sliderTextInput)
Shiny.addCustomMessageHandler('restoreSliderValue', function(message) {
    if (!message.id || message.selected === undefined || message.selected === null) return;

    const questionId = message.id;
    const selected = message.selected;

    // Function to attempt slider update with retries
    function attemptSliderUpdate(retryCount) {
        // Find the slider input element
        const sliderInput = $('#' + questionId);
        if (sliderInput.length === 0) {
            if (retryCount < 5) {
                setTimeout(function() { attemptSliderUpdate(retryCount + 1); }, 100);
            }
            return;
        }

        // Get the ion.rangeSlider instance
        const sliderData = sliderInput.data('ionRangeSlider');
        if (sliderData) {
            // Get the choices from the slider
            const choices = sliderData.options.values || [];

            // Find the index of the selected value
            const selectedIndex = choices.indexOf(selected);

            if (selectedIndex >= 0) {
                // Update the slider position
                sliderData.update({
                    from: selectedIndex
                });
            }
        } else if (retryCount < 5) {
            // Slider not initialized yet, retry
            setTimeout(function() { attemptSliderUpdate(retryCount + 1); }, 100);
        }
    }

    // Start with initial delay then attempt update
    setTimeout(function() { attemptSliderUpdate(0); }, 150);
});

// Handle restoration of button-style inputs (mc_buttons and mc_multiple_buttons)
Shiny.addCustomMessageHandler('restoreButtonValue', function(message) {
    if (!message.id || !message.value) return;

    const questionId = message.id;
    const value = message.value;
    const type = message.type || 'radio';

    setTimeout(function() {
        if (type === 'radio') {
            // For mc_buttons (radio button groups)
            // Find the button with this value and trigger a click
            const button = $('#' + questionId + ' input[type="radio"][value="' + value + '"]').closest('.btn');
            if (button.length > 0) {
                // Manually update the visual state
                $('#' + questionId + ' .btn').removeClass('active');
                button.addClass('active');
                // Ensure the radio input is checked
                $('#' + questionId + ' input[type="radio"][value="' + value + '"]').prop('checked', true);
            }
        } else if (type === 'checkbox') {
            // For mc_multiple_buttons (checkbox button groups)
            const values = Array.isArray(value) ? value : [value];
            // Clear all selections first
            $('#' + questionId + ' .btn').removeClass('active');
            $('#' + questionId + ' input[type="checkbox"]').prop('checked', false);
            // Set each selected value
            values.forEach(function(val) {
                const button = $('#' + questionId + ' input[type="checkbox"][value="' + val + '"]').closest('.btn');
                if (button.length > 0) {
                    button.addClass('active');
                    $('#' + questionId + ' input[type="checkbox"][value="' + val + '"]').prop('checked', true);
                }
            });
        }
    }, 150); // Delay to ensure DOM is ready
});