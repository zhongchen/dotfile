;;
;; toggle between eshell and emacs. C-c t is the kbd.
(fset 'eshell-visor-on
"\M-xeshell\n")
(fset 'eshell-visor-off
"\M-xbury-buffer\n")
 
;; assumes using reset-ui based layout
(defun toggle-eshell-visor ()
(interactive)
(if (string= "eshell-mode" (eval 'major-mode))
(execute-kbd-macro (symbol-function 'eshell-visor-off))
(execute-kbd-macro (symbol-function 'eshell-visor-on))))

(global-set-key (kbd "C-c t") 'toggle-eshell-visor)

(provide 'init-misc)
