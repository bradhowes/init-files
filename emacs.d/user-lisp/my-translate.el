;;; my-translate.el --- Google translate functions -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(defun my/translate--do (start end source target)
  "Translate text between START and END from SOURCE language to TARGET language."
  (message "Running translate %s %s..." source target)
  (let* ((program (expand-file-name "~/src/translator/translate"))
         (delete t)
         (destination t)
         (display t))
    (call-process-region start end program delete destination display source target)))

;;;###autoload
(defun my/translate-en-fr (start end)
  "Translate text in region START to END from English to French."
  (interactive "r")
  (my/translate--do start end "en" "fr"))

;;;###autoload
(defun my/translate-fr-en (start end)
  "Translate text in region START to END from French to English."
  (interactive "r")
  (my/translate--do start end "fr" "en"))

;;; This is a test.

(provide 'my-translate)

;;; my-translate.el ends here.
