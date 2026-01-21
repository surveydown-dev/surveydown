// Initialize ClipboardJS for code copy buttons in dynamically loaded Shiny content
(function() {
  'use strict';

  // Check if ClipboardJS is available (loaded from Quarto's head)
  function initCodeCopy() {
    if (typeof ClipboardJS === 'undefined') {
      console.warn('ClipboardJS not available, code copy disabled');
      return;
    }

    // Helper to check if element is a code annotation
    const isCodeAnnotation = (el) => {
      for (const clz of el.classList) {
        if (clz.startsWith('code-annotation-')) {
          return true;
        }
      }
      return false;
    };

    // Get text to copy from code block
    const getTextToCopy = function(trigger) {
      // Find the code element - it could be a sibling or nested differently
      let codeEl = null;

      // Try to find the code element within the sourceCode div
      const sourceCode = trigger.closest('.sourceCode');
      if (sourceCode) {
        codeEl = sourceCode.querySelector('code');
      }

      // Fallback: try previous sibling approach (Quarto's default structure)
      if (!codeEl) {
        const prevSibling = trigger.previousElementSibling;
        if (prevSibling) {
          codeEl = prevSibling.querySelector('code') || prevSibling;
        }
      }

      if (!codeEl) {
        console.warn('Could not find code element for copy button');
        return '';
      }

      // Clone and remove annotations
      const clonedCode = codeEl.cloneNode(true);
      for (const childEl of clonedCode.children) {
        if (isCodeAnnotation(childEl)) {
          childEl.remove();
        }
      }
      return clonedCode.innerText;
    };

    // Success callback
    const onCopySuccess = function(e) {
      const button = e.trigger;
      button.blur();
      button.classList.add('code-copy-button-checked');
      const currentTitle = button.getAttribute('title');
      button.setAttribute('title', 'Copied!');

      // Use Bootstrap tooltip if available
      let tooltip;
      if (window.bootstrap && bootstrap.Tooltip) {
        button.setAttribute('data-bs-toggle', 'tooltip');
        button.setAttribute('data-bs-placement', 'left');
        button.setAttribute('data-bs-title', 'Copied!');
        tooltip = new bootstrap.Tooltip(button, {
          trigger: 'manual',
          customClass: 'code-copy-button-tooltip',
          offset: [0, -8]
        });
        tooltip.show();
      }

      setTimeout(function() {
        if (tooltip) {
          tooltip.hide();
          button.removeAttribute('data-bs-title');
          button.removeAttribute('data-bs-toggle');
          button.removeAttribute('data-bs-placement');
        }
        button.setAttribute('title', currentTitle);
        button.classList.remove('code-copy-button-checked');
      }, 1000);

      e.clearSelection();
    };

    // Destroy existing clipboard instances to avoid duplicates
    const existingClipboards = window._surveydownClipboards || [];
    existingClipboards.forEach(function(cb) {
      if (cb && typeof cb.destroy === 'function') {
        cb.destroy();
      }
    });
    window._surveydownClipboards = [];

    // Initialize ClipboardJS for all code copy buttons
    const buttons = document.querySelectorAll('.code-copy-button');
    if (buttons.length > 0) {
      const clipboard = new ClipboardJS('.code-copy-button', {
        text: getTextToCopy
      });
      clipboard.on('success', onCopySuccess);
      clipboard.on('error', function(e) {
        console.error('Copy failed:', e);
      });
      window._surveydownClipboards.push(clipboard);
    }
  }

  // Initialize on DOM ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initCodeCopy);
  } else {
    initCodeCopy();
  }

  // Re-initialize when Shiny updates content
  $(document).on('shiny:value', function(event) {
    // Small delay to let DOM update
    setTimeout(initCodeCopy, 100);
  });

  // Also use MutationObserver as backup for any DOM changes
  const observer = new MutationObserver(function(mutations) {
    // Check if any mutations added code-copy-button elements
    let needsInit = false;
    mutations.forEach(function(mutation) {
      mutation.addedNodes.forEach(function(node) {
        if (node.nodeType === 1) { // Element node
          if (node.classList && node.classList.contains('code-copy-button')) {
            needsInit = true;
          } else if (node.querySelector && node.querySelector('.code-copy-button')) {
            needsInit = true;
          }
        }
      });
    });
    if (needsInit) {
      setTimeout(initCodeCopy, 100);
    }
  });

  // Start observing when DOM is ready
  function startObserver() {
    const targetNode = document.getElementById('quarto-content') || document.body;
    observer.observe(targetNode, { childList: true, subtree: true });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', startObserver);
  } else {
    startObserver();
  }
})();
