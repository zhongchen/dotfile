;;default display position/size based on display resolution
;;warning: assumption that the 24/22 " displays are oriented
;; above the laptop's display
;; Haven't found a way to check multiple monitor
;; relative orientation via emacs yet...
;;
;; a frame is passed in when firing on after-make-frame-fuctions,
;; but not necessary when calling interactively
(defun reset-ui (&optional frame)
(if frame
(select-frame frame))
(interactive)
(smex-initialize)
(load-theme 'solarized-dark t)
(delete-other-windows)
(set-cursor-color "deeppink")
(set-face-background 'modeline-inactive "gray10")
(if (window-system)
(cond
((= 1050 (display-pixel-height)); 22" display
(set-frame-size (selected-frame) 163 71)
(set-frame-position (selected-frame) 0 -1050))
((= 1200 (display-pixel-height)); 24" display
(set-frame-size (selected-frame) 163 76)
(set-frame-position (selected-frame) 0 -1200))
(t ; laptop runs 1440x900
(set-frame-size (selected-frame) 163 53)
(set-frame-position (selected-frame) 0 0))))
(split-window-horizontally))
 
;; fires when an emacs frame is created (emacsclient)
;; invoke like this ( on osx):
;; emacsclient -c -n; osascript -e "tell application \"Emacs\" to activate"
(add-hook 'after-make-frame-functions 'reset-ui)
 
;; Opean two emacs windows by default.
;; hook for setting up UI when not running in daemon mode
(add-hook 'emacs-startup-hook 'reset-ui)
 
;; toggle between eshell and emacs. C-c t is the kbd.
(fset 'eshell-visor-on
"\C-x1\M-xeshell\n")
(fset 'eshell-visor-off
"\C-x3\M-xbury-buffer\n\C-xo\M-xbury-buffer\n\M-xswap-windows")
 
;; assumes using reset-ui based layout
(defun toggle-eshell-visor ()
(interactive)
(if (string= "eshell-mode" (eval 'major-mode))
(execute-kbd-macro (symbol-function 'eshell-visor-off))
(execute-kbd-macro (symbol-function 'eshell-visor-on))))

(global-set-key (kbd "\C-cu") 'reset-ui)
(global-set-key (kbd "C-c t") 'toggle-eshell-visor)
(provide init-misc)
