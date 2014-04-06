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
(evil-leader/set-key "SPC" 'ace-jump-mode-pop-mark) ;jump back
(global-set-key (kbd "C-c w") 'ace-jump-mode)
(global-set-key (kbd "C-c W") 'ace-jump-char-mode)
(global-set-key (kbd "C-c l") 'ace-jump-line-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode-pop-mark)

;general leader keybindings
(evil-leader/set-key "e" 'find-file)
(evil-leader/set-key "b" 'switch-to-buffer)
(evil-leader/set-key "k" 'kill-buffer)

;enable iedit
(global-set-key (kbd "C-c ;") 'iedit-mode)
(evil-leader/set-key "i" 'iedit-mode)

;xcscope keybinding
(define-key evil-normal-state-map (kbd "SPC") 'cscope-show-entry-other-window)

(message "successfully finish keybinding process")
(provide 'init-keybinding)
