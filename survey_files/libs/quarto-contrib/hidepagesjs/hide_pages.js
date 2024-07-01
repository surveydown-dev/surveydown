// Define a global function to hide all pages
window.hideAllPages = function() {
  var pages = document.querySelectorAll("div.sd-page");
  pages.forEach(function(page) {
    page.style.display = 'none';
  });
};

// Call the function on initial load to hide all pages except the first
document.addEventListener("DOMContentLoaded", function() {
  window.hideAllPages();
  document.querySelectorAll("div.sd-page")[0].style.display = 'block';
});
