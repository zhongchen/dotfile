; set up evil 
; enable evil-leader before evil
(require 'evil-leader)
(global-evil-leader-mode 1)
(evil-leader/set-leader ",")

;enable evil
(require 'evil)
(evil-mode 1)

;evil match it
(require 'evil-matchit)
(global-evil-matchit-mode 1)

; I can put useful commands in the file as well
(provide 'init-evil)
