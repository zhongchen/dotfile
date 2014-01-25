;install el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get") 
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(require 'el-get)

; enalbe git shallow clone
; the commit history may be truncated
; and I might not push to the depot
(setq el-get-git-shallow-clone t)

;my own recipes, which will override the default ones
(add-to-list 'el-get-recipe-path "~/.emacs.d/local_recipe")

;; set local recipes
(setq
 el-get-sources
 '((:name smex                                ; a better (ido like) M-x
         :after (progn
                 (setq smex-save-file "~/.emacs.d/.smex-items")
                 (global-set-key (kbd "M-x") 'smex)
                 (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit                                ; git meet emacs, and a binding
         :after (progn
                 (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change                ; move pointer back to last change
         :after (progn
                 (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

(setq 
  my_packages
  '(el-get 
    color-theme
    color-theme-tango

    ; vim
    evil
    evil-surround
    evil-numbers
    evil-nerd-commenter

    ; Completion
    auto-complete
    auto-complete-clang
    ;auto-complete-yasnippet
    ;yasnippet
    
    ; syntax checking
    flymake

    ; 
    cl-lib
    
    ))


(setq my_packages
      (append
	my_packages
	(mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my_packages)
(el-get 'sync my_packages)

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

;enable evil
(require 'evil)
(evil-mode 1)

; load some random els.
(add-to-list 'load-path "~/.emacs.d/packages/")

; enable google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(require 'smooth-scrolling)
(require 'whitespace)

;auto complete
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
;; (ac-set-trigger-key "TAB")
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
 /usr/include/c++/4.8
 /usr/include/x86_64-linux-gnu/c++/4.8
 /usr/include/c++/4.8/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.8/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include")))

(message "done with initialziation")
(provide 'init)
