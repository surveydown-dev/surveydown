<script>
// Initialize the progress bar
window.initializeProgressBar = function() {
  const progressBar = document.getElementById("progress-bar");
  const progressContainer = document.getElementById("progress-container");

  // Debug: Check if elements are found
  console.log('Progress Bar Element:', progressBar);
  console.log('Progress Container Element:', progressContainer);

  // Assume we have access to the config object
  console.log('Config Object:', config);
  if (config.progress_bar === "none") {
    progressContainer.style.display = "none";
  } else {
    progressContainer.style.display = "block";
    if (config.progress_bar === "top") {
      progressContainer.style.position = "fixed";
      progressContainer.style.top = "0";
      progressContainer.style.width = "100%";
    }
    // Add more positioning logic as needed
  }

  // Function to update progress bar
  window.updateProgressBar = function(currentPage, totalPages) {
    const progress = (currentPage / totalPages) * 100;
    progressBar.style.width = progress + "%";
  }

  // Example usage
  const totalPages = config.page_ids.length;
  let currentPage = 1; // This should be dynamically set based on the survey's state
  window.updateProgressBar(currentPage, totalPages);
};

// Initialize the progress bar on initial load
document.addEventListener("DOMContentLoaded", function() {
  window.initializeProgressBar();
});
</script>
