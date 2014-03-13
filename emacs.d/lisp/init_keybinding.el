(message "keybinding mapping process")

;remapping help-command
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'help-command)

;remapping winner redo undo 
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

;leader keybinding
(evil-leader/set-key "w" 'ace-jump-mode)
(evil-leader/set-key "W" 'ace-jump-char-mode)
(evil-leader/set-key "l" 'ace-jump-line-mode)
(global-set-key (kbd "C-c w") 'ace-jump-mode)
(global-set-key (kbd "C-c W") 'ace-jump-char-mode)
(global-set-key (kbd "C-c l") 'ace-jump-line-mode)

;general leader keybindings
(evil-leader/set-key "e" 'find-file)
(evil-leader/set-key "b" 'switch-to-buffer)
(evil-leader/set-key "k" 'kill-buffer)

(message "successfully finish keybinding process")
(provide 'init_keybinding)
