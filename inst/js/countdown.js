function startCountdown(delay, redirectFunc, countdownId, uniqueId) {
    var countdown = delay;
    var countdownTimer;

    function updateCountdown() {
        countdown--;
        if (countdown <= 0) {
            clearInterval(countdownTimer);
            redirectFunc();
        } else {
            document.getElementById(countdownId).textContent = countdown;
        }
    }

    function startTimer() {
        countdownTimer = setInterval(updateCountdown, 1000);
    }

    // Start countdown when this element becomes visible
    var observer = new IntersectionObserver(function(entries) {
        if(entries[0].isIntersecting === true) {
            startTimer();
            observer.disconnect();
        }
    }, { threshold: [0] });

    observer.observe(document.getElementById(uniqueId));
}
