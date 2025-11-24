const surveydownCookies = {
    set: function(sessionId) {
        try {
            const date = new Date();
            date.setTime(date.getTime() + (30 * 24 * 60 * 60 * 1000)); // 30 days
            const cookieValue = "surveydown_session=" + sessionId +
                              ";expires=" + date.toUTCString() +
                              ";path=/;SameSite=Strict";
            document.cookie = cookieValue;
            console.log("Session cookie set successfully:", cookieValue);
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
            currentData[pageId] = pageData;  // pageData contains both answers and timestamps
            
            const date = new Date();
            date.setTime(date.getTime() + (30 * 24 * 60 * 60 * 1000));
            const cookieValue = "surveydown_answers=" + JSON.stringify(currentData) +
                              ";expires=" + date.toUTCString() +
                              ";path=/;SameSite=Strict";
            document.cookie = cookieValue;
            console.log("Answer data set for page:", pageId);
            
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
            // Clear session cookie
            document.cookie = "surveydown_session=;expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/;SameSite=Strict";
            
            // Clear answer data cookie
            document.cookie = "surveydown_answers=;expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/;SameSite=Strict";
            
            console.log("Surveydown cookies cleared successfully");
        } catch (e) {
            console.error("Error clearing cookies:", e);
        }
    },

    forceRestart: function() {
        try {
            // Clear cookies and force restart
            this.clear();
            console.log("Forcing survey restart by clearing cookies");
        } catch (e) {
            console.error("Error forcing restart:", e);
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

Shiny.addCustomMessageHandler('clearCookies', function(message) {
    surveydownCookies.clear();
});

Shiny.addCustomMessageHandler('forceRestart', function(message) {
    surveydownCookies.forceRestart();
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