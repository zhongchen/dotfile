;the file contains the envrionment setting
(message "start the envionment setting")

;; on to the visual settings
(setq inhibit-splash-screen t)              ; no splash screen, thanks
(line-number-mode 1)                        ; have line numbers and
(column-number-mode 1)                      ; column numbers in the mode line

(tool-bar-mode -1)                          ; no tool bar with icons
(scroll-bar-mode -1)                        ; no scroll bars

(global-hl-line-mode)                        ; highlight current line
(global-linum-mode 1)                        ; add line numbers on the left

(menu-bar-mode -1)
(normal-erase-is-backspace-mode 1)

; no start up message
(setq inhibit-startup-message t)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; M-x shell is a nice shell interface to use, let's make it colorful. If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                 (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit autoface-default :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :family "monaco"))))
 '(column-marker-1 ((t (:background "red"))))
 '(diff-added ((t (:foreground "cyan"))))
 '(flymake-errline ((((class color) (background light)) (:background "Red"))))
 '(font-lock-comment-face ((((class color) (min-colors 8) (background light)) (:foreground "red"))))
 '(fundamental-mode-default ((t (:inherit default))))
 '(highlight ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
 '(isearch ((((class color) (min-colors 8)) (:background "yellow" :foreground "black"))))
 '(linum ((t (:foreground "black" :weight bold))))
 '(region ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
 '(secondary-selection ((((class color) (min-colors 8)) (:background "gray" :foreground "cyan"))))
 '(show-paren-match ((((class color) (background light)) (:background "black"))))
 '(vertical-border ((t nil)))
)

(message "finish the environment setting")
(provide 'environment_setting)
