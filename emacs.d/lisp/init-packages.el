;version 24 or up use package.el
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  ;include a third party package resource
  (setq package-archives
	'(("gnu" . "http://elpa.gnu.org/packages/") 
	  ("melpa" . "http://melpa.milkbox.net/packages/")))
)

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
; and push is limited
(setq el-get-git-shallow-clone t)

;my own recipes, which will override the default ones
(add-to-list 'el-get-recipe-path "~/.emacs.d/local-recipe")

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
  my-packages
  '(el-get 
    color-theme
    solarized-theme
    nodejs-repl
    ctags
    xcscope
   
    ; vim
    evil
    evil-surround
    evil-numbers
    evil-nerd-commenter
    evil-matchit
    evil-leader

    ; Completion
    auto-complete
    auto-complete-clang
    auto-complete-yasnippet
    yasnippet
    
    ; syntax checking
    flycheck
    
    ; multiple windows
    window-numbering

    undo-tree

    ;python IDE 
    python-mode
    ropemacs
    pymacs
    jedi

    ;orthodox file manager
    ;sunrise-commander
     
    ;javascript IDE
    js2-mode
    js2-refactor
    coffee-mode
    zencoding-mode
    js-comint
    expand-region
    smart-forward
    powerline
    ;jade-mode

    ;smart indention and auto-completion
    smart-tab

    ; multi-occurence editing
    iedit

    ace-jump-mode
    cl-lib))

(setq my-packages
  (append
    my-packages
    (mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)
(provide 'init-packages)
