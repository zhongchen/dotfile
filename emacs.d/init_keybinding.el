(message "keybinding process")
;remapping help-command
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(message "successfully finish keybinding process")
(provide 'init_keybinding)
