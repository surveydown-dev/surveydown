function keepAlive() {
  Shiny.onInputChange('keepAlive', Math.random());
}

// Run keepAlive every 60 seconds
setInterval(keepAlive, 60000);