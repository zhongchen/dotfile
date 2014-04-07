;;; record time
(setq emacs-load-start-time (current-time))

(setq debug-on-error t)    ; now you should get a backtrace

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(setq *spell-check-support-enabled* t)
(setq *macbook-pro-support-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *win32* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )
(setq *xemacs* (featurep 'xemacs) )
(setq *emacs23* (and (not *xemacs*) (or (>= emacs-major-version 23))) )
(setq *emacs24* (and (not *xemacs*) (or (>= emacs-major-version 24))) )


(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'linum)
(require 'dired-x)
(require 'compile)
(require 'init-packages)
(require 'init-environment-setting)
(require 'init-keybinding)
(require 'cl-lib)

; enable color theme
(require 'color-theme-mac-classic)
(color-theme-mac-classic)
o
; initialize ido-mode
(require 'init-ido)

; initialize evil-mode
(require 'init-evil)

(require 'ctags)
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)
(setq tags-revert-without-query t)
(global-set-key (kbd "M-.") 'ctags-search)

; multi-occurence edit
(require 'iedit)

; load some random els.
(add-to-list 'load-path "~/.emacs.d/packages/")

; enable google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(require 'smooth-scrolling)
(require 'whitespace)

; multiple windows
(require 'window-numbering)
(window-numbering-mode 1)

; redo/undo  window configurations
(winner-mode 1)

;quick jump
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back :)" t)

; auto-complete setup
(require 'init-auto-complete)

(require 'xcscope)
(cscope-setup)

;;yasnippet initialization
;(require 'yasnippet)
;(yas-global-mode 1)
;(yas/initialize)
;(yas/load-directory "~/.emac.d/el-get/yasnippet/snippets/text-mode")
;(add-to-list 'ac-sources 'ac-source-yasnippet)

; set up python IDE
(require 'init-python)

; set up javascript IDE
(require 'init-javascript)

;enalbe syntax check
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)


; random stuff
(require 'init-misc)
(message "done with initialziation")
(when (require 'time-date nil t)
   (message "Emacs startup time: %d seconds."
    (time-to-seconds (time-since emacs-load-start-time))))
