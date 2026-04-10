// Move every .sd-attr-wrap out of its parent <label> so clicking the ?
// button never activates the associated radio/checkbox input.
function relocateAttrWraps() {
  attrObserver.disconnect();
  document.querySelectorAll('label .sd-attr-wrap').forEach(function(wrap) {
    var label = wrap.closest('label');
    if (!label) return;
    label.parentNode.insertBefore(wrap, label.nextSibling);
    label.parentNode.classList.add('sd-has-attr');
  });
  attrObserver.observe(document.documentElement, { childList: true, subtree: true });
}

var attrObserver = new MutationObserver(relocateAttrWraps);
attrObserver.observe(document.documentElement, { childList: true, subtree: true });

function sdAttrToggle(btn, event) {
  event.stopPropagation();

  var popup = btn.nextElementSibling;
  var isPinned = popup.classList.contains('sd-attr-open');

  document.querySelectorAll('.sd-attr-popup.sd-attr-open').forEach(function(p) {
    p.classList.remove('sd-attr-open');
    p.previousElementSibling.classList.remove('sd-attr-active');
  });

  if (!isPinned) {
    popup.classList.add('sd-attr-open');
    btn.classList.add('sd-attr-active');
  }
}

document.addEventListener('click', function() {
  document.querySelectorAll('.sd-attr-popup.sd-attr-open').forEach(function(p) {
    p.classList.remove('sd-attr-open');
    p.previousElementSibling.classList.remove('sd-attr-active');
  });
});
