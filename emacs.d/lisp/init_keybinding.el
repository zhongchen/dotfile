(message "keybinding mapping process")
;remapping help-command
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'help-command)

(message "successfully finish keybinding process")
(provide 'init_keybinding)
