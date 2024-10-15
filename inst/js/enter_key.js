$(document).on('shiny:sessioninitialized', function() {
    var isModalOpen = false;
    $(document).on('shown.bs.modal hidden.bs.modal', function(e) {
        isModalOpen = e.type === 'shown';
    });

    $(document).on('keydown', function(event) {
        if (event.key === 'Enter' && !event.repeat &&
            !$(event.target).is('textarea, input[type="text"], input[type="number"], select, input[type="date"]')) {
            event.preventDefault();
            if (isModalOpen) {
                var $exitButton = $('.modal.show .modal-footer .btn:contains("Submit and Exit"), .modal.show .modal-footer .btn:contains("Exit")').first();
                if ($exitButton.length) {
                    $exitButton.click();
                }
            } else {
                $('.sd-enter-button:visible').first().click();
            }
        }
    });
});
