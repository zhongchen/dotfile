(add-to-list 'load-path "~/.emacs.d/el-get/el-get") ; only without ELPA/el-get
(add-to-list 'load-path "~/.emacs.d/evil") ; only without ELPA/el-get
(add-to-list 'load-path "~/.emacs.d") ; only without ELPA/el-get
(add-to-list 'load-path "~/.emacs.d/auto-complete") ; only without ELPA/el-get
(add-to-list 'load-path "~/.emacs.d/auto-complete/lib/popup") ; only without ELPA/el-get
(add-to-list 'load-path "~/.emacs.d/auto-complete/lib/fuzzy") ; only without ELPA/el-get

(require 'init)

;(unless (require 'el-get nil 'noerror)
;  (with-current-buffer
;      (url-retrieve-synchronously
;       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;    (goto-char (point-max))
;    (eval-print-last-sexp)))

;(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;(el-get 'sync)


(require 'evil)
(evil-mode 1)

(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process)
)

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)

(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;import google c++ style
(add-to-list 'load-path "~/.emacs.d/google-c-style")
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
