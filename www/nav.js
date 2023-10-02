$(document).ready(function() {
  // Initially, hide all sections except the first one
  console.log("Document is ready.");

  $("section").hide();
  $("#welcome").show();
  console.log("Sections are hidden, welcome section is shown.");

  // Variables
  let currentPage = 1;
  let maxPage = $("section").length;

  // Add navigation controls, you can customize this further
  $("body").append("<button id='prev'>Previous</button>");
  $("body").append("<button id='next'>Next</button>");

  $("#prev").click(function() {
    if (currentPage > 1) {
      $(`#pg${currentPage}`).hide();
      currentPage--;
      $(`#pg${currentPage}`).show();
    }
  });

  $("#next").click(function() {
    if (currentPage < maxPage) {
      $(`#pg${currentPage}`).hide();
      currentPage++;
      $(`#pg${currentPage}`).show();
    }
  });
});
