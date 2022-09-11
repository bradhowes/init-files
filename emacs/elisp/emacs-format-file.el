;;; File: emacs-format-file
;; http://www.cslab.pepperdine.edu/warford/BatchIndentationEmacs.html
;; to be run as --
;; emacs -batch *.[ch] -l ~/src/scripts/emacs-format-file -f emacs-format-function

(defun emacs-format-function ()
  (untabify (point-min) (point-max))
  (save-buffer))
