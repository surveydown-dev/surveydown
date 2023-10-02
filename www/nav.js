document.addEventListener("DOMContentLoaded", function() {

    // Define the current page index
    let currentPage = 0;

    // Function to show the specific page and hide others
    function showPage(pageIndex) {
        // Get all the sections
        let sections = document.querySelectorAll('section[id^="pg"]');

        // Hide all sections
        sections.forEach(function(section) {
            section.style.display = "none";
        });

        // Show the specific section
        if (sections[pageIndex]) {
            sections[pageIndex].style.display = "block";
        }
    }

    // Initial page setup: Show the first page
    showPage(currentPage);

    // Add event listener to next and previous buttons
    document.getElementById('next_btn').addEventListener('click', function() {
        currentPage++;
        showPage(currentPage);
    });

    document.getElementById('prev_btn').addEventListener('click', function() {
        if (currentPage > 0) {
            currentPage--;
            showPage(currentPage);
        }
    });

    // Listen to messages from the parent window (Shiny app)
    window.addEventListener("message", function(event) {
        if (event.data === "next") {
            currentPage++;
            showPage(currentPage);
        } else if (event.data === "previous" && currentPage > 0) {
            currentPage--;
            showPage(currentPage);
        }
    });
});
