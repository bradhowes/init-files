;;; my-gnus.el --- custom setting for Gnus -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'gnus)

;;;###autoload
(defun my/gnus-initialize ()
  "Initialize gnus."
  (interactive)

  ;; Set iCloud IMAP as the primary mail source
  (setopt gnus-select-method
          '(nnimap "iCloud"
                   (nnimap-address "imap.mail.me.com")
                   (nnimap-server-port 993)
                   (nnimap-stream ssl)
                   (nnimap-user "bradhowes@mac.com"))

          ;; Outgoing email via Apple SMTP server
          send-mail-function 'smtpmail-send-it
          message-send-mail-function 'smtpmail-send-it
          smtpmail-smtp-server "smtp.mail.me.com"
          smtpmail-smtp-service 587
          smtpmail-stream-type 'starttls

          ;; Email addressing
          user-full-name "Brad Howes"
          user-mail-address "bradhowes@mac.com")

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (gnus))

(provide 'my-gnus)

;;; my-gnus.el ends here.
