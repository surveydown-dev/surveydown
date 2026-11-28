// accent.js
// A selection is marked in the accent color, so the accent has to stand out
// from the page. A theme whose accent is almost its page color (slate: dark
// gray on dark gray) gets its accent mixed toward the text color until the two
// can be told apart. Every other theme is left alone.

(function() {
  'use strict';

  var NEAR = 0.15;  // OKLab distance under which the accent is lost on the page
  var CLEAR = 0.3;  // distance the adjusted accent has to reach, if the text gets that far
  var SENTINEL = '#010203';

  var canvas = document.createElement('canvas');
  canvas.width = canvas.height = 1;
  var ctx = canvas.getContext('2d', { willReadFrequently: true });

  // The color a custom property resolves to, as the browser writes it
  function token(name) {
    var probe = document.createElement('span');
    probe.style.color = 'var(' + name + ')';
    document.body.appendChild(probe);
    var color = getComputedStyle(probe).color;
    probe.remove();
    return color;
  }

  // A CSS color as OKLab [L, a, b], or null when the canvas cannot read it
  function oklab(color) {
    ctx.fillStyle = SENTINEL;
    ctx.fillStyle = color;
    if (ctx.fillStyle === SENTINEL) return null;
    ctx.clearRect(0, 0, 1, 1);
    ctx.fillRect(0, 0, 1, 1);
    var pixel = ctx.getImageData(0, 0, 1, 1).data;
    var c = [pixel[0], pixel[1], pixel[2]].map(function(v) {
      v /= 255;
      return v <= 0.04045 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4);
    });
    var l = Math.cbrt(0.4122214708 * c[0] + 0.5363325363 * c[1] + 0.0514459929 * c[2]);
    var m = Math.cbrt(0.2119034982 * c[0] + 0.6806995451 * c[1] + 0.1073969566 * c[2]);
    var s = Math.cbrt(0.0883024619 * c[0] + 0.2817188376 * c[1] + 0.6299787005 * c[2]);
    return [
      0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s,
      1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s,
      0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
    ];
  }

  function distance(a, b) {
    return Math.hypot(a[0] - b[0], a[1] - b[1], a[2] - b[2]);
  }

  function start() {
    if (!ctx) return;
    var accentColor = token('--theme-color');
    var inkColor = token('--sd-ink');
    var accent = oklab(accentColor);
    var ink = oklab(inkColor);
    var paper = oklab(token('--sd-paper'));
    if (!accent || !ink || !paper || distance(accent, paper) >= NEAR) return;

    // The smallest share of ink that sets the accent apart from the page. On a
    // low-contrast theme the text itself is close to the page, so the goal is
    // most of the way to the text.
    var goal = Math.min(CLEAR, 0.8 * distance(ink, paper));
    var share = 0;
    var mixed = accent;
    while (share < 90 && distance(mixed, paper) < goal) {
      share += 5;
      mixed = accent.map(function(v, i) { return v + (ink[i] - v) * share / 100; });
    }
    document.documentElement.style.setProperty(
      '--theme-color',
      'color-mix(in oklab, ' + accentColor + ' ' + (100 - share) + '%, ' + inkColor + ')'
    );
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }
})();
