function updateProgressBar(progress) {
  var progressBar = document.getElementById("progress");
  if (progressBar) {
    progressBar.style.width = progress + "%";
  }
}

Shiny.addCustomMessageHandler('updateProgressBar', function(progress) {
  updateProgressBar(progress);
});
