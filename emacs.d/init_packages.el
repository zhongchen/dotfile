;version 24 or up use package.el
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  ;include a third party package resource
  (setq package-archives
	'(("gnu" . "http://elpa.gnu.org/packages/") 
	  ("melpa" . "http://melpa.milkbox.net/packages/")))
)

(provide 'init_packages)
