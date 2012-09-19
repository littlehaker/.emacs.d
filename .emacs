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
(require 'tomorrow-night-theme)
(require 'lua-mode)

(require 'autopair)
(autopair-global-mode)
;(pc-selection-mode)

(global-linum-mode)
(require 'whole-line-or-region)
(whole-line-or-region-mode)

(require 'auto-complete)
(global-auto-complete-mode)

(require 'highlight-symbol)
(highlight-symbol-mode)
(global-set-key [(f3)] 'highlight-symbol-next)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key [(ctrl f3)] 'highlight-symbol-at-point)

(global-set-key [(f1)] 'ibuffer)
(global-set-key (kbd "C-c C-g") 'goto-line)
(global-set-key (kbd "C-x C-z") 'magit-status)
(global-set-key (kbd "M-w") 'whole-line-or-region-kill-ring-save)

(require 'yasnippet)
(yas-global-mode 1)

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
 '(js-indent-level 2)
 '(js2-basic-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-background-chunk-submode1 ((t (:background "gray20")))))
