;;; my-notes.el ---  -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'denote)
(require 'consult-notes)
(require 'consult-notes-denote)
(require 'my-env)

(defun my/notes--denote-format-keywords-for-md-front-matter (keywords)
  "Custom KEYWORDS formatter for keystrokecountdown.com markdown files.
The default Markdown keyword formatter puts each keyword in double-quotes,
separates them with a \", \" and surrounds the result with square brackets.
Here, we just separate them by a comma."
  (format "%s" (mapconcat (lambda (k) k) keywords ", ")))

;;;###autoload
(defun my/notes-denote-hook ()
  "Custom hook for denote."
  (push '(markdown-brh
          :extension ".md"
          :date-value-function denote-date-rfc3339
          :date-value-reverse-function denote-extract-date-from-front-matter
          :front-matter denote-yaml-front-matter
          :title-key-regexp "^title\\s-*:"
          :title-value-function denote-trim-whitespace
          :title-value-reverse-function denote-trim-whitespace
          :keywords-key-regexp "^tags\\s-*:"
          :keywords-value-function my/notes--denote-format-keywords-for-md-front-matter
          :keywords-value-reverse-function denote-extract-keywords-from-front-matter
          :link denote-md-link-format
          :link-in-context-regexp denote-md-link-in-context-regexp)
        denote-file-types))

(defun my/notes--denote-items (directory)
  "Fetch the denote files in DIRECTORY."
  (let* ((max-width 0)
         (denote-directory directory)
         (cands (mapcar (lambda (f)
                          (let* ((id (denote-retrieve-filename-identifier f))
                                 (title-1 (or (denote-retrieve-title-value f (denote-filetype-heuristics f))
                                              (denote-retrieve-filename-title f)))
                                 (title (if consult-notes-denote-display-id
                                            (concat id " " title-1)
                                          title-1))
                                 (keywords (denote-extract-keywords-from-path f)))
                            (let ((current-width (string-width title)))
                              (when (> current-width max-width)
                                (setq max-width (+ 24 current-width))))
                            (propertize title 'denote-path f 'denote-keywords keywords)))
                        (funcall consult-notes-denote-files-function))))
    (mapcar (lambda (c)
              (let* ((keywords (get-text-property 0 'denote-keywords c))
                     (path (get-text-property 0 'denote-path c))
                     (dirs (directory-file-name (file-relative-name (file-name-directory path) denote-directory))))
                (concat c
                        ;; align keywords
                        (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
                        (format "%18s"
                                (if keywords
                                    (concat (propertize "#" 'face 'consult-notes-name)
                                            (propertize (mapconcat 'identity keywords " ") 'face 'consult-notes-name))
                                  ""))
                        (when consult-notes-denote-dir (format "%18s" (propertize (concat "/" dirs)
                                                                                  'face
                                                                                  'consult-notes-name))))))
              cands)))

(defun my/notes--create-denote-source (name key directory)
  "Create a `consult-notes' source with NAME, KEY, and DIRECTORY."
  (list :name (propertize name 'face 'consult-notes-sep)
        :narrow key
        :category 'consult-notes-category
        :annotate #'consult-notes-denote--annotate
        :items (lambda () (my/notes--denote-items directory))
        :state #'consult-notes-denote--state
        :new #'consult-notes-denote--new-note))

;;;###autoload
(defun my/notes-consult-notes-hook ()
  "Custom hook for `consult-notes'."
  (let* ((sources `(("personal" ?r ,(my/denote-directory-personal))
                    ("work" ?w ,(my/denote-directory-work)))))
    (dolist (source sources)
      (push (apply #'my/notes--create-denote-source source) consult-notes-all-sources))))

(provide 'my-notes)

;;; my-notes.el ends here.
