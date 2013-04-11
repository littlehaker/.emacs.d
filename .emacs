(package-initialize)
(setq load-path (cons "~/.emacs.d/themes/" load-path))
(setq load-path (cons "~/.emacs.d/" load-path))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; ;; nxhtml
;; (load "~/.emacs.d/nxhtml/autostart")

(require 'color-theme)
                                        ;(require 'zen-and-art-theme)
;; (require 'color-theme-solarized)
;; (color-theme-solarized-light)
;; (require 'tomorrow-night-bright-theme)
;; (require 'tomorrow-night-theme)
(require 'molokai-theme)
;; (require 'monokai-theme)
;; (color-theme-solarized-dark)
;; (require 'subatomic-theme)

(require 'lua-mode)

;; (require 'autopair)
;; (autopair-global-mode)

;;;;;;;;;
;; global
(require 'smartparens-config)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
(define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
(define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
(define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;<" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;>"))

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;; (require 'paredit)
;; (paredit-mode)

(highlight-indentation-mode)
(highlight-parentheses-mode)
                                        ;(pc-selection-mode)

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(global-linum-mode)
(require 'whole-line-or-region)
(whole-line-or-region-mode)

(require 'auto-complete)
(global-auto-complete-mode)

(helm-mode 1)
(global-set-key (kbd "M-p") 'helm-occur)


(global-set-key (kbd "C-=") 'er/expand-region)

;; (require 'main-line)

(require 'powerline)

;; (setq powerline-arrow-shape 'arrow)   ;; the default
;; (custom-set-faces
;;  '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
;;  '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-warnline ((t (:underline t))))
 '(js2-warning ((t (:underline "saddle brown"))))
 '(main-line-active1 ((t (:inherit mode-line :background "grey22" :foreground "white"))) t)
 '(main-line-active2 ((t (:inherit mode-line :background "grey40" :foreground "white"))) t)
 '(mode-line ((t (:foreground "black" :background "OliveDrab3" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil))))
 '(mumamo-background-chunk-major ((t nil)) t)
 '(mumamo-background-chunk-submode1 ((t (:background "gray20"))) t)
 '(powerline-active1 ((t (:inherit mode-line :background "grey22" :foreground "white"))) t)
 '(powerline-active2 ((t (:inherit mode-line :background "grey40" :foreground "white"))) t)
 '(web-mode-html-tag-face ((t (:foreground "RoyalBlue1")))))
;; (setq powerline-color1 "grey22")
;; (setq powerline-color2 "grey40")
;; (setq powerline-color1 "grey22")
;; (setq powerline-color2 "grey40")
;; (powerline-default-center)

;; (setq powerline-color1 "#657b83")
;; (setq powerline-color2 "#839496")

;; (set-face-attribute 'mode-line nil
;;                     :foreground "#fdf6e3"
;;                     :background "#859900"
;;                     :box nil)
;; (set-face-attribute 'mode-line-inactive nil
;;                     :box nil)

;; (require 'smart-mode-line)
;; (sml/setup)

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  ;; (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
  (if (not (region-active-p))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)
(global-set-key (kbd "C-;") 'comment-dwim)

;; (defun hilite-todos ()
;;   (highlight-lines-matching-regexp "\\<\\(FIXME\\|WRITEME\\|WRITEME!\\|TODO\\|BUG\\):?" 
;;        'hi-green-b)
;; )

;; (add-hook '$WHATEVER-mode-hook 'hilite-todos)

(require 'fixme-mode)
;; (require 'fixmee)
;; (global-fixmee-mode 1)

(require 'smart-forward)
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key [(f2)] 'helm-projectile)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c y") 'helm-c-yas-complete)
(global-set-key (kbd "C-c a") 'helm-apt)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map 
                [remap pcomplete]
                'helm-esh-pcomplete)))


(global-hl-line-mode)

(require 'highlight-symbol)
(highlight-symbol-mode)
(global-set-key [(f3)] 'highlight-symbol-next)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key [(ctrl f3)] 'highlight-symbol-at-point)

;; (global-set-key [(f1)] 'ibuffer)
(global-set-key [(f1)] 'helm-mini)

(global-set-key (kbd "C-c C-g") 'goto-line)
(global-set-key (kbd "C-x C-z") 'magit-status)
(global-set-key (kbd "M-w") 'whole-line-or-region-kill-ring-save)

;; (global-set-key (kbd "C-,") 'goto-line)

(require 'yasnippet)
(yas-global-mode 1)

;; (require 'main-line)

;; 彩色括号
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; 绑定按键
(global-set-key [(ctrl tab)] 'other-window)

;; 禁用auto-fill-mode
(setq auto-fill-mode -1)
(setq fill-column 99999)
(setq-default fill-column 99999)

;; 字体
;; (set-default-font "WenQuanYi Micro Hei Mono-13.5")
(set-default-font "Monaco for Powerline-12")

;; (set-default-font "URW Gothic L-13.5")
;; (set-default-font "Droid Sans-13.5")
;; (set-default-font "WenQuanYi Micro Hei-14")
;; (set-default-font "Ubuntu-15")


(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)) ) ;M-k kills to the left

;; evil-mode
;; (require 'evil-mode)
(global-set-key [(f12)] 'evil-mode)
;; 绑定auto-complete和yasnippet

;; (require 'auto-complete-yasnippet)

;; (defface ac-yasnippet-candidate-face
;;   '((t (:background "sandybrown" :foreground "black")))
;;   "Face for yasnippet candidate.")

;; (defface ac-yasnippet-selection-face
;;   '((t (:background "coral3" :foreground "white")))
;;   "Face for the yasnippet selected candidate.")

;; (defvar ac-source-yasnippet
;;   '((candidates . ac-yasnippet-candidate)
;;     (action . yas/expand)
;;     (candidate-face . ac-yasnippet-candidate-face)
;;     (selection-face . ac-yasnippet-selection-face))
;;   "Source for Yasnippet.")

;; (set-default 'ac-sources
;;              '(
;;                ;; ac-source-semantic
;;                ac-source-yasnippet
;;                ac-source-abbrev
;;                ac-source-words-in-buffer
;;                ac-source-words-in-all-buffer
;;                ;; ac-source-imenu
;;                ac-source-files-in-current-dir
;;                ac-source-filename
;;                )
;;              )

;; (require 'ace-jump-mode)
;; (global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; ;; 绑定super键
;; ;; setting the PC keyboard's various keys to
;; ;; Super or Hyper, for emacs running on Windows.
;; (setq w32-pass-lwindow-to-system nil 
;;       w32-pass-rwindow-to-system nil 
;;       w32-pass-apps-to-system nil 
;;       w32-lwindow-modifier 'super ; Left Windows key 
;;       w32-rwindow-modifier 'super ; Right Windows key 
;;       w32-apps-modifier 'hyper) ; Menu key


;; (setq js-indent-level 2)
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(coffee-command "livescript")
 '(coffee-tab-width 2)
 '(css-indent-offset 2)
 '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "fe0a47cc3952fede574527a1c28ddf3a1af381fc1fb5843ca60d22e4c841011a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "60a2ebd7effefeb960f61bc4772afd8b1ae4ea48fae4d732864ab9647c92093a" "93451314424e9fac1473a27800b563c7b166b4f8c91400384ab5b994c846318d" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "61240c08e41d353c7d21fa61da97c8dbd3eb0feffd55cc0bdc9f0ff0ed048274" "580a24d8f262e9dddbcee7572430ccdb3d9a4088d03e853137c65023d0b04e4d" default)))
 '(fci-rule-color "#383838")
 '(flymake-jslint-command "jslint")
 '(flymake-log-level 0)
 '(grep-command "grep -rn . ")
 '(httpd-port 8000)
 '(inf-mongo-command "/usr/bin/mongo 127.0.0.1:27017")
 '(js-indent-level 2)
 '(main-line-separator-style (quote zigzag)))

(add-hook 
 'coffee-mode-hook
 (lambda () (define-key coffee-mode-map (kbd "<backtab>") 'coffee-unindent)))



;; Shift the selected region right if distance is positive, left if
;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one 
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:

;; (require 'coco-mode)
;; (require 'livescript-mode)
;; (add-hook 'livescript-mode-hook
;;           (lambda ()
;;             (define-key livescript-mode-map (kbd "<backtab>") 'livescript-unindent)))
;; (defun livescript-unindent ()
;;   (interactive)
;;   (if mark-active
;;       ;; (coffee-unindent-block)
;;     (progn
;;       (indent-line-to (- (current-indentation) 2)))))
;; (define-key livescript-mode-map "\C-c\C-l" 'livescript-compile-buffer)

(require 'fold-dwim)
(global-set-key [(super tab)] 'fold-dwim-toggle)
(global-set-key (kbd "C-`") 'fold-dwim-toggle-selective-display)


;;;###autoload
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-hook 'js2-mode-hook (lambda () (setq autopair-dont-activate t)))

(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")

(add-to-list 'auto-mode-alist '("\\.ls\\'" . coffee-mode))

(global-undo-tree-mode)
(global-set-key (kbd "C-?") 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo-tree-undo)

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)


(defun coffee-unindent-block ()
  (shift-region (- coffee-tab-width))
  (setq deactivate-mark nil))
(defun coffee-unindent ()
  (interactive)
  (if mark-active
      (coffee-unindent-block)
    (progn
      (indent-line-to (- (current-indentation) coffee-tab-width)))))



(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)] 
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(eval-after-load "autopair"
  '(progn
     (autopair-global-mode 1)

     (setq my-autopair-off-modes '(js2-mode))
     (dolist (m my-autopair-off-modes)
       (add-hook (intern (concat (symbol-name m) "-hook"))
                 #'(lambda () 
                     (setq autopair-dont-activate t)
                     (autopair-mode -1))))
     ))

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (message "format successfully"))

(global-set-key [f7] 'indent-buffer)

;; (require 'mon-css-color)
;; (css-color-global-mode)

;; (add-hook 'js-mode-hook 'flymake-jslint-load)
(add-hook 'js-mode-hook 'js2-minor-mode)
;; (require 'flymake-jshint)
;; (add-hook 'javascript-mode-hook
;;      (lambda () (flymake-mode t)))

;;直接Ctrl＋回车 进入新的一行，光标不必在行尾
(global-set-key [C-return] '(lambda () 
                              (interactive) 
                              (move-end-of-line 1) 
                              (newline-and-indent))) 

;; (wrap-region-global-mode)

;; CSS and Rainbow modes 
(defun all-css-modes() (css-mode) (rainbow-mode)) 
;; Load both major and minor modes in one call based on file type 
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes)) 


(require 'flymake-css)
(add-hook 'css-mode-hook 'flymake-css-load)
;; (require 'css-format)

(require 'helm-themes)
;; (require 'tron-theme)
;; (load-theme 'leuven t)
(require 'helm-c-yasnippet)

(require 'requirejs-mode)
(add-hook 'js-mode-hook (lambda () (requirejs-mode)))
(add-hook 'html-mode-hook 'zencoding-mode)

;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
;; (add-hook 'web-mode-hook 'zencoding-mode)
(add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))

;; (require 'mmm-mode)
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
