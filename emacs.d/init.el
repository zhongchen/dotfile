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

;auto complete
(require 'auto-complete-config)
(ac-config-default)

; load some random els.
(add-to-list 'load-path "~/.emacs.d/packages/")

;enable google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(require 'smooth-scrolling)
(require 'whitespace)

(message "done with initialziation")
(provide 'init)
