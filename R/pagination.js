<script>
  document.addEventListener("DOMContentLoaded", function() {
    // Select all divs with IDs that start with 'page-'
    var pages = document.querySelectorAll("div[id^='page-']");

    // Iterate over the NodeList and hide each element, except for the first page
    pages.forEach(function(page) {
      if (page.id !== 'page-1') {
        page.style.display = 'none';
      }
    });
  });
</script>