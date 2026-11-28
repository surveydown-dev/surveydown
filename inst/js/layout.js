// layout.js
// Layout that depends on measurement, which CSS alone cannot decide:
//
//   1. Choice buttons (mc_buttons, mc_multiple_buttons) that need more than
//      one line: the options are spread evenly over the lines and every line
//      gets the same width.
//   2. Navigation buttons: when their labels do not fit on one line, the
//      secondary buttons shrink to icons.
//   3. Matrix questions: they scroll sideways only when their columns do not
//      fit, and their column headers are marked while they are stuck to the
//      top of the window.
//   4. Footer: the center slot is put in the middle of the page when there
//      is room, and the page is told the footer's height.
//
// Each function clears what it set before it measures, so it can run again
// whenever the page, the window or the fonts change.

(function() {
  'use strict';

  var BUTTONS = '.radio-group-buttons > .btn-group-container-sw, .checkbox-group-buttons > .btn-group-container-sw';
  var NAVS = '.sd-nav-container';
  var MATRICES = '.matrix-question-scroll';
  var FOOTERS = '.footer > .footer-content';
  var CORNERS = ['sd-tl', 'sd-tr', 'sd-bl', 'sd-br'];
  var phone = window.matchMedia('(max-width: 575px)');

  // --- 1. Choice buttons ---------------------------------------------------

  // Split widths `w` into `rows` consecutive lines no wider than `room`, so
  // that the widest line is as narrow as possible and, among those splits,
  // the lines are as even as possible. Returns the index after each line.
  function splitEvenly(w, gap, room, rows) {
    var n = w.length;
    var sum = [0];
    for (var i = 0; i < n; i++) sum.push(sum[i] + w[i]);
    var line = function(i, j) { return sum[j] - sum[i] + (j - i - 1) * gap; };

    // best[r][j]: the first j options on r lines
    var best = [];
    for (var r = 0; r <= rows; r++) best.push([]);
    best[0][0] = { widest: 0, squares: 0, from: -1 };
    for (r = 1; r <= rows; r++) {
      for (var j = r; j <= n; j++) {
        for (i = r - 1; i < j; i++) {
          var before = best[r - 1][i];
          var width = line(i, j);
          if (!before || width > room + 0.5) continue;
          var widest = Math.max(before.widest, width);
          var squares = before.squares + width * width;
          var now = best[r][j];
          if (!now || widest < now.widest - 0.5 ||
              (widest < now.widest + 0.5 && squares < now.squares)) {
            best[r][j] = { widest: widest, squares: squares, from: i };
          }
        }
      }
    }
    if (!best[rows][n]) return null;
    var ends = [];
    for (r = rows, j = n; r > 0; r--) {
      ends.unshift(j);
      j = best[r][j].from;
    }
    return ends;
  }

  function layoutButtons(box) {
    var items = [];
    for (var k = 0; k < box.children.length; k++) {
      if (box.children[k].classList.contains('btn-group')) items.push(box.children[k]);
    }

    box.classList.remove('sd-rows');
    box.style.width = '';
    items.forEach(function(item) {
      item.style.flex = '';
      CORNERS.forEach(function(c) { item.classList.remove(c); });
    });

    // Options with pictures or several lines are cards (sd-cards), which
    // share the row equally instead
    var cards = !!box.querySelector('img, br');
    box.classList.toggle('sd-cards', cards);

    var host = box.parentElement;
    var style = getComputedStyle(box);
    if (items.length < 2 || !host.clientWidth ||
        style.flexWrap === 'nowrap' || style.flexDirection.indexOf('column') === 0 ||
        cards) return;

    // Natural widths, margins included
    box.classList.add('sd-measure');
    var margins = [];
    var w = items.map(function(item) {
      var s = getComputedStyle(item);
      var margin = parseFloat(s.marginLeft) + parseFloat(s.marginRight);
      margins.push(margin);
      return Math.ceil(item.getBoundingClientRect().width * 10) / 10 + margin;
    });
    box.classList.remove('sd-measure');

    var gap = parseFloat(style.columnGap) || 0;
    var padding = parseFloat(style.paddingLeft) + parseFloat(style.paddingRight);
    var room = host.clientWidth - padding;
    w = w.map(function(x) { return Math.min(x, room); });

    // Lines needed when each line is filled before the next one starts
    var rows = 1, used = 0;
    w.forEach(function(x, i) {
      var next = used ? used + gap + x : x;
      if (i > 0 && next > room + 0.5) { rows++; used = x; } else { used = next; }
    });
    if (rows < 2) return;

    var ends = splitEvenly(w, gap, room, rows);
    if (!ends) return;

    var lines = [], start = 0;
    ends.forEach(function(end) {
      var total = 0;
      for (var i = start; i < end; i++) total += w[i];
      lines.push({ start: start, end: end, total: total, width: total + (end - start - 1) * gap });
      start = end;
    });

    // On a phone the block takes the full width; otherwise it is as wide as
    // its widest line and sits centered
    var width = phone.matches ? room : Math.max.apply(null, lines.map(function(l) { return l.width; }));

    box.classList.add('sd-rows');
    box.style.width = (width + padding) + 'px';
    lines.forEach(function(l, row) {
      var extra = width - l.width;
      for (var i = l.start; i < l.end; i++) {
        // Each button's share of its line; a line filled this way leaves no
        // room for the next button, which is what breaks the line there
        var basis = w[i] - margins[i] + extra * w[i] / l.total;
        items[i].style.flex = '1 1 ' + (Math.floor(basis * 10) / 10 - 0.1) + 'px';
      }
      if (row === 0) {
        items[l.start].classList.add('sd-tl');
        items[l.end - 1].classList.add('sd-tr');
      }
      if (row === lines.length - 1) {
        items[l.start].classList.add('sd-bl');
        items[l.end - 1].classList.add('sd-br');
      }
    });
  }

  // --- 2. Navigation -------------------------------------------------------

  function navWidth(nav, buttons) {
    nav.classList.add('sd-measure');
    var gap = parseFloat(getComputedStyle(nav).columnGap) || 0;
    var total = (buttons.length - 1) * gap;
    buttons.forEach(function(b) { total += b.getBoundingClientRect().width; });
    nav.classList.remove('sd-measure');
    return total;
  }

  function layoutNav(nav) {
    var buttons = Array.prototype.slice.call(nav.querySelectorAll(':scope > .btn'));
    nav.classList.remove('sd-nav-tight', 'sd-nav-tighter');
    buttons.forEach(function(b) {
      if (b.dataset.sdTitle) { b.removeAttribute('title'); delete b.dataset.sdTitle; }
    });
    if (buttons.length < 2 || !nav.clientWidth) return;

    if (navWidth(nav, buttons) <= nav.clientWidth + 0.5) return;
    nav.classList.add('sd-nav-tight');
    if (nav.querySelector(':scope > .sd-nav-next') &&
        navWidth(nav, buttons) > nav.clientWidth + 0.5) {
      nav.classList.add('sd-nav-tighter');
    }

    // An icon button says what it does on hover
    buttons.forEach(function(b) {
      if (parseFloat(getComputedStyle(b).fontSize) === 0 && !b.title) {
        b.title = b.textContent.replace(/[←→]/g, '').trim();
        b.dataset.sdTitle = 'true';
      }
    });
  }

  // --- 3. Matrix -----------------------------------------------------------

  function layoutMatrix(scroller) {
    scroller.classList.remove('sd-fits');
    if (!scroller.clientWidth) return;
    scroller.style.setProperty('--sd-matrix-view', scroller.clientWidth + 'px');
    if (scroller.scrollWidth <= scroller.clientWidth + 1) scroller.classList.add('sd-fits');
  }

  // The column headers stick to the top of the window while a long matrix
  // scrolls past. They are stuck once they have left the top of their table.
  var stuckScheduled = false;

  function markStuck() {
    stuckScheduled = false;
    document.querySelectorAll(MATRICES).forEach(function(scroller) {
      var table = scroller.querySelector('table.matrix-question');
      var th = table && table.querySelector('th');
      var head = th && th.closest('tbody');
      if (!head) return;
      var away = head.getBoundingClientRect().top - table.getBoundingClientRect().top;
      head.classList.toggle('sd-stuck', head.offsetHeight > 0 && away > 0.5);
    });
  }

  function scheduleStuck() {
    if (stuckScheduled) return;
    stuckScheduled = true;
    requestAnimationFrame(markStuck);
  }

  // --- 4. Footer -----------------------------------------------------------

  // The slots share one line while their text fits; the center slot is then
  // put in the middle of the page if the outer slots can have equal shares.
  // When the text has to wrap, the slots sit side by side or one above the
  // other, whichever makes the shorter footer. The page learns the footer's
  // height, which grows when its text wraps.
  function layoutFooter(content) {
    content.classList.remove('sd-footer-even', 'sd-footer-stack');
    if (!content.clientWidth) return;
    var room = content.clientWidth + 0.5;
    var slots = content.children.length;

    content.classList.add('sd-measure');
    var width = function(name) {
      var slot = content.querySelector(':scope > .footer-' + name);
      return slot ? slot.getBoundingClientRect().width : 0;
    };
    var left = width('left'), center = width('center'), right = width('right');
    var gap = parseFloat(getComputedStyle(content).columnGap) || 0;
    content.classList.remove('sd-measure');

    if (left + center + right + (slots - 1) * gap <= room) {
      if (center && 2 * Math.max(left, right) + center + 2 * gap <= room) {
        content.classList.add('sd-footer-even');
      }
    } else if (slots > 1) {
      var sideBySide = content.offsetHeight;
      content.classList.add('sd-footer-stack');
      if (content.offsetHeight >= sideBySide) content.classList.remove('sd-footer-stack');
    }

    var footer = content.closest('.footer');
    if (footer) {
      document.documentElement.style.setProperty('--sd-footer-height', footer.offsetHeight + 'px');
    }
  }

  // --- Running it ----------------------------------------------------------

  var watched = new Set();
  var resizeObserver = typeof ResizeObserver === 'undefined' ? null : new ResizeObserver(schedule);
  var scheduled = false;

  function watch(el) {
    if (!resizeObserver || watched.has(el)) return;
    watched.add(el);
    resizeObserver.observe(el);
  }

  function layoutAll() {
    scheduled = false;
    watched.forEach(function(el) {
      if (!el.isConnected) { resizeObserver.unobserve(el); watched.delete(el); }
    });
    document.querySelectorAll(BUTTONS).forEach(function(box) {
      watch(box.parentElement);
      layoutButtons(box);
    });
    document.querySelectorAll(NAVS).forEach(function(nav) {
      watch(nav);
      layoutNav(nav);
    });
    document.querySelectorAll(MATRICES).forEach(function(scroller) {
      watch(scroller);
      layoutMatrix(scroller);
    });
    document.querySelectorAll(FOOTERS).forEach(function(content) {
      watch(content.parentElement);
      layoutFooter(content);
    });
    markStuck();
  }

  function schedule() {
    if (scheduled) return;
    scheduled = true;
    requestAnimationFrame(layoutAll);
  }

  // New pages and re-rendered questions arrive as added nodes
  var ALL = BUTTONS + ', ' + NAVS + ', ' + MATRICES;
  var mutationObserver = new MutationObserver(function(mutations) {
    for (var m = 0; m < mutations.length; m++) {
      var added = mutations[m].addedNodes;
      for (var i = 0; i < added.length; i++) {
        var node = added[i];
        if (node.nodeType !== 1) continue;
        if (node.matches(ALL) || node.querySelector(ALL) || node.closest('.btn-group-container-sw')) {
          schedule();
          return;
        }
      }
    }
  });

  function start() {
    mutationObserver.observe(document.body, { childList: true, subtree: true });
    if (phone.addEventListener) phone.addEventListener('change', schedule);
    window.addEventListener('resize', schedule);
    window.addEventListener('scroll', scheduleStuck, { passive: true });
    if (document.fonts && document.fonts.ready) document.fonts.ready.then(schedule);
    if (document.fonts && document.fonts.addEventListener) {
      document.fonts.addEventListener('loadingdone', schedule);
    }
    schedule();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }
})();
