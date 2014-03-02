(message "keybinding mapping process")

;remapping help-command
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'help-command)

;remapping winner redo undo 
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

(message "successfully finish keybinding process")
(provide 'init_keybinding)
