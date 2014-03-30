;;; record time
(setq emacs-load-start-time (current-time))

(setq max-specpdl-size 10000)  ; default is 1000, reduce the backtrace level
(setq debug-on-error t)    ; now you should get a backtrace
(setq max-lisp-eval-depth 10000)

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
(require 'cl-lib)
(require 'init-packages)
(require 'init-environment-setting)
(require 'init-keybinding)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)

; enable evil-leader before evil
(require 'evil-leader)
(global-evil-leader-mode 1)
(evil-leader/set-leader ",")

;enable evil
(require 'evil)
(evil-mode 1)

;evil match it
(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'ctags)
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)
(setq tags-revert-without-query t)
(global-set-key (kbd "M-.") 'ctags-search)

; multi-occurence edit
(require 'iedit)

; enable color theme
(require 'color-theme-mac-classic)
(color-theme-mac-classic)

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
(require 'ace-jump-mode)

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
