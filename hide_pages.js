<script>
document.addEventListener("DOMContentLoaded", function() {
  // Hide all pages initially except first page
  var pages = document.querySelectorAll("div.sd-page");
  pages.forEach(function(page) {
    page.style.display = 'none';
  });
  if (pages.length > 0) {
    pages[0].style.display = 'block'; // Show the first page
  }
});
</script>
