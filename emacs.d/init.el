;record time
(setq emacs-load-start-time (current-time))

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


(require 'init_packages)
(require 'init_environment_setting)
(require 'init_keybinding)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'linum)
(require 'dired-x)
(require 'compile)

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

; set up python IDE
(require 'init_python)

; set up javascript IDE
(require 'init_javascript)

;auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
(ac-config-default)
(setq ac-auto-show-menu t)
(setq ac-modes '(c-mode cc-mode c++-mode))
(setq ac-use-fuzzy t)

; enable clang-complete
(require 'auto-complete-clang)

;(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
(ac-set-trigger-key "TAB")
(define-key ac-mode-map  [(control tab)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
	      (split-string
               "
 /usr/include/c++/4.9
 /usr/include/x86_64-linux-gnu/c++/4.9
 /usr/include/c++/4.9/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.9/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include")))

; yasnippet initialization
;(require 'yasnippet)
;(yas/initialize)
;(yas/load-directory ~/.emac.d/el-get/yasnippet/snippets/text-mode")
;(add-to-list 'ac-sources 'ac-source-yasnippet)

(message "done with initialziation")
(when (require 'time-date nil t)
   (message "Emacs startup time: %d seconds."
    (time-to-seconds (time-since emacs-load-start-time))))
