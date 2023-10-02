document.addEventListener("DOMContentLoaded", function() {

    const sections = document.querySelectorAll("section");
    let currentPage = 0;

    // Function to show only the specified page
    function showPage(pageIndex) {
        sections.forEach((sec, idx) => {
            if (idx === pageIndex) {
                sec.style.display = "block";
            } else {
                sec.style.display = "none";
            }
        });
    }

    // Initial setup: show only the first page
    showPage(currentPage);

    // Add event listeners to navigation buttons
    document.querySelectorAll(".prev-btn").forEach((btn, idx) => {
        btn.addEventListener("click", function() {
            if (currentPage > 0) {
                currentPage--;
                showPage(currentPage);
            }
        });
    });

    document.querySelectorAll(".next-btn").forEach((btn, idx) => {
        btn.addEventListener("click", function() {
            if (currentPage < sections.length - 1) {
                currentPage++;
                showPage(currentPage);
            }
        });
    });
});
