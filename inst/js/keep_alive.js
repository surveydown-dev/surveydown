// Initialize interval with a default value
let keepAliveInterval = 60000; // 60 seconds default

function keepAlive() {
  Shiny.onInputChange('keepAlive', Math.random());
}

// Function to start the keepAlive interval
function startKeepAlive() {
  setInterval(keepAlive, keepAliveInterval);
}

// Function to update the interval
Shiny.addCustomMessageHandler("updateKeepAliveInterval", function(interval) {
  keepAliveInterval = interval;
  // Clear existing interval and start a new one
  clearInterval(keepAliveInterval);
  startKeepAlive();
});

// Start the initial interval
startKeepAlive();