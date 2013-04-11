This package provide a semantic way of using tab characters in
source code: tabs for indentation, spaces for alignment.

Installation:

To use, save smart-tabs-mode.el to a a directory on your load-path
(e.g., ~/.emacs.d/elisp), then add the following to your .emacs file:

 (autoload 'smart-tabs-mode "smart-tabs-mode"
   "Intelligently indent with tabs, align with spaces!")
 (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
 (autoload 'smart-tabs-advice "smart-tabs-mode")

Then, for each language you want to use smart tabs, set up a hook
and advice like so:

 (add-hook 'MODE-HOOK 'smart-tabs-mode-enable)
 (smart-tabs-advice INDENT-FUNC TAB-WIDTH-VAR)

Note that it might be preferable to delay calling smart-tabs-advice
until after the major mode is loaded and evaluated:

(eval-after-load 'MODE-FEATURE
  '(smart-tabs-advice INDENT-FUNC TAB-WIDTH-VAR))

Or:

 (add-hook 'MODE-HOOK (lambda ()
                        (smart-tabs-mode-enable)
                        (smart-tabs-advice INDENT-FUNC TAB-WIDTH-VAR)))

Here are some specific examples for a few popular languages which
can be enabled by 'smart-tab-insinuate':

 ;; Load all the following in one pass
 (smart-tabs-insinuate 'c 'javascript 'cperl 'python 'ruby)

 ;; C/C++
 (add-hook 'c-mode-hook 'smart-tabs-mode-enable)
 (smart-tabs-advice c-indent-line c-basic-offset)
 (smart-tabs-advice c-indent-region c-basic-offset)

 ;; JavaScript
 (add-hook 'js2-mode-hook 'smart-tabs-mode-enable)
 (smart-tabs-advice js2-indent-line js2-basic-offset)

 ;; Perl (cperl-mode)
 (add-hook 'cperl-mode-hook 'smart-tabs-mode-enable)
 (smart-tabs-advice cperl-indent-line cperl-indent-level)

 ;; Python
 (add-hook 'python-mode-hook 'smart-tabs-mode-enable)
 (smart-tabs-advice python-indent-line-1 python-indent)

 ;; Ruby
 (add-hook 'ruby-mode-hook 'smart-tabs-mode-enable)
 (smart-tabs-advice ruby-indent-line ruby-indent-level)

This package is derived from <http://www.emacswiki.org/emacs/SmartTabs>
as modified by John Croisant (jacius), along with Julien Fontanet and
Tomita Hiroshi (tomykaira).

Modification history is at <https://github.com/jcsalomon/smarttabs>.
