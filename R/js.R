hide_all_pages <- function() {
    js_code <- "
    (function() {
        var pages = document.querySelectorAll('.sd-page');
        pages.forEach(function(page) {
            page.style.display = 'none';
        });
    })();
    "
    runjs(js_code)
}

show_first_page <- function() {
    js_code <- "
    (function() {
        var pages = document.querySelectorAll('.sd-page');
        if (pages.length > 0) {
            pages[0].style.display = 'block';
        }
    })();
    "
    runjs(js_code)
}

