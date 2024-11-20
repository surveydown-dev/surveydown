// cookies.js
const surveydownCookies = {
    set: function(sessionId) {
        try {
            const date = new Date();
            date.setTime(date.getTime() + (30 * 24 * 60 * 60 * 1000)); // 30 days
            const cookieValue = "surveydown_session=" + sessionId +
                              ";expires=" + date.toUTCString() +
                              ";path=/;SameSite=Strict";
            document.cookie = cookieValue;
            console.log("Cookie set successfully:", cookieValue);
            console.log("Current cookies:", document.cookie);
        } catch (e) {
            console.error("Error setting cookie:", e);
        }
    },

    get: function() {
        try {
            console.log("All cookies:", document.cookie);
            const name = "surveydown_session=";
            const decodedCookie = decodeURIComponent(document.cookie);
            const ca = decodedCookie.split(';');
            for(let i = 0; i < ca.length; i++) {
                let c = ca[i];
                while (c.charAt(0) == ' ') {
                    c = c.substring(1);
                }
                if (c.indexOf(name) == 0) {
                    const sessionId = c.substring(name.length);
                    console.log("Found session cookie:", sessionId);
                    return sessionId;
                }
            }
            console.log("No session cookie found");
            return null;
        } catch (e) {
            console.error("Error getting cookie:", e);
            return null;
        }
    }
};

// Set cookie handler
Shiny.addCustomMessageHandler('setCookie', function(message) {
    console.log("Received setCookie message:", message);
    if (message.sessionId) {
        surveydownCookies.set(message.sessionId);
        // Verify the cookie was set
        const verifyId = surveydownCookies.get();
        console.log("Verified cookie after setting:", verifyId);
    }
});

// Add handler for triggering input changes
Shiny.addCustomMessageHandler('triggerInputChange', function(message) {
    var inputEl = $('#' + message.inputId);
    if (inputEl.length) {
        inputEl.trigger('change');
    }
});

// Initialize on document ready - with retry mechanism
$(document).ready(function() {
    function initializeSession(retryCount = 0) {
        const sessionId = surveydownCookies.get();
        console.log("Initializing with session ID:", sessionId, "Retry count:", retryCount);

        if (sessionId) {
            Shiny.setInputValue('stored_session_id', sessionId, {priority: 'event'});
        } else if (retryCount < 3) {
            // Retry after a short delay
            setTimeout(() => initializeSession(retryCount + 1), 100);
        }
    }

    initializeSession();
});

// Also handle Shiny reconnections
$(document).on('shiny:connected', function(event) {
    const sessionId = surveydownCookies.get();
    console.log("Shiny reconnected, session ID:", sessionId);
    if (sessionId) {
        Shiny.setInputValue('stored_session_id', sessionId, {priority: 'event'});
    }
});
