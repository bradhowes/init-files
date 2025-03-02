;;; package -- my-display -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Misc functions for managing screens / frames
;;; Code:

(require 'my-constants)

(defun my/screen-layout ()
  "Identify current screen layout.
Uses result from `display-pixel-width' to determine what monitors
there are.  Better would be to use `display-monitor-attributes-list'
like done in `my/frame-top'.

Returns one of the follow symbols based on width:

- `my/screen-laptop' -- only laptop screen
- `my/screen-4k' -- only 4K monitor.
- `my/screen-laptop-4k' -- laptop screen + 4K monitor.
- `my/screen-4k-4k' -- two 4K monitors.
- `my/screen-laptop-4k-4k' -- laptop screen + 2 4K monitors.
- `my/screen-terminal' -- unknown screen."
  (declare (side-effect-free t))
  (let* ((width (display-pixel-width nil))
         (value (cond ((= width my/laptop-screen-width) my/screen-laptop)
                      ((= width my/4k-screen-width) my/screen-4k)
                      ((= width (+ my/laptop-screen-width my/4k-screen-width)) my/screen-laptop-4k)
                      ((= width (* my/4k-screen-width 2)) my/screen-4k-4k)
                      ((= width (+ my/laptop-screen-width (* 2 my/4k-screen-width))) my/screen-laptop-4k-4k)
                      (t my/screen-terminal))))
    (message "my/screen-layout: %s" value)
    value))

(defun my/is-laptop (layout)
  "T if LAYOUT is laptop."
  (declare (side-effect-free t))
  (eq layout my/screen-laptop))

(defun my/is-4k (layout)
  "T if LAYOUT is kind with at least 4K area."
  (declare (side-effect-free t))
  (memq layout '(my/screen-4k my/screen-laptop-4k my/screen-4k-4k my/screen-laptop-4k-4k)))

(defun my/rows (layout)
  "The number of rows to show in a frame shown on LAYOUT."
  (declare (side-effect-free t))
  (if (my/is-4k layout) 103 (if (my/is-laptop layout) 88 40)))

(defun my/cols (layout)
  "The number of columns to show in a frame shown on LAYOUT."
  (declare (side-effect-free t))
  (if (or (my/is-4k layout) (my/is-laptop layout)) 132 80))

(defun my/frame-pixel-width (layout)
  "Width in pixels of a normal frame shown on LAYOUT.
These values are hard-coded based on current settings.
Probably a better way to figure this out."
  (declare (side-effect-free t))
  (if (my/is-4k layout) (if my/is-macosx 1338 1338) 944))

(defun my/frame-initial-left (layout)
  "Pixels to use for the `left' of a frame on LAYOUT.
This is to be used for the `initial-frame-alist' configuration."
  (declare (side-effect-free t))
  ;; Use the first external monitor if there is one.
  (if (memq layout '(my/screen-laptop-4k my/screen-laptop-4k-4k))
      (+ 2056 (* my/screen-4k-pick my/4k-screen-width))
    0))

(defun my/frame-default-left (layout)
  "Pixels to use for the `left' of a frame on LAYOUT.
This is to be use for the `default-frame-aliat' configuration."
  (declare (side-effect-free t))
  (+ (my/frame-initial-left layout) (my/frame-pixel-width layout)))

(defun my/frame-third-left (layout)
  "The offset to the `alt' window based on LAYOUT.
This is not used in any particular `*-frame-alist' but it is used
by custom commands that reposition a frame to be flush with the
right-side of the active screen that is being used to host
Emacs frames."
  (declare (side-effect-free t))
  (- (+ (my/frame-initial-left layout)
        (if (eq layout my/screen-laptop)
            my/laptop-screen-width
          my/4k-screen-width))
     (my/frame-pixel-width layout)))

(defun my/frame-top ()
  "The top of the display area.
NOTE: this assumes that the laptop display if present is,
is on the left of any monitors."
  (declare (side-effect-free t))
  (let ((settings (display-monitor-attributes-list)))
    (+ (nth 2 (if (eq (length settings) 1)
                  (car (car settings))
                (car (car (cdr settings)))))
       ;; Apply an offset to show the title bar when running under VcXsrv on Windows
       (if my/is-x-windows-on-win 30 0))))

(defun my/initial-frame-alist (layout)
  "Make alist to use for the initial frame on LAYOUT."
  (declare (side-effect-free t))
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-initial-left layout))))

(defun my/default-frame-alist (layout)
  "Make alist to use for the default frame on LAYOUT."
  (declare (side-effect-free t))
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-default-left layout))))

(defun my/align-right-frame-alist (layout)
  "The alist to use extra frame on LAYOUT.
The frame will appear on the far right of the display area."
  (declare (side-effect-free t))
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-third-left layout))))

(defun my/update-screen-frame-alists (layout)
  "Update frame alists for current LAYOUT."
  (setq initial-frame-alist (my/initial-frame-alist layout)
        default-frame-alist (my/default-frame-alist layout))
  (message "initial-frame: %s" initial-frame-alist)
  (message "default-frame: %s" default-frame-alist))

(defun my/font-size (layout)
  "The font size to use based on the LAYOUT."
  (declare (side-effect-free t))
  (if (my/is-4k layout) 16 12))

(defun my/setup-font (layout)
  "Install the desired font in the default face for LAYOUT."
  (set-face-attribute 'default nil :font (font-spec :family my/font-name :size (my/font-size layout))))

(defun my/screen-layout-changed ()
  "Recalculate values based on screen layout."
  (interactive)
  (let ((layout (my/screen-layout)))
    (message "screen layout: %s" layout)
    (my/setup-font layout)
    (my/update-screen-frame-alists layout)))

(defun my/pick-screen-4k (screen)
  "Set the 4K SCREEN to use to host future Emacs frames.
It does not affect existing frames."
  (interactive "NScreen:")
  (custom-set-variables (list 'my/screen-4k-pick screen))
  (custom-save-all)
  (my/screen-layout-changed))

(defun my/reset-frame-left ()
  "Reset frame size and position for left frame."
  (interactive)
  (let ((layout (my/screen-layout)))
    (modify-frame-parameters (window-frame (get-buffer-window)) (my/initial-frame-alist layout))))

(defun my/reset-frame-right ()
  "Reset frame size and position for right frame."
  (interactive)
  (let ((layout (my/screen-layout)))
    (modify-frame-parameters (window-frame (get-buffer-window)) (my/default-frame-alist layout))))

(defun my/reset-frame-right-display ()
  "Reset frame size and position for right side of display frame."
  (interactive)
  (let ((layout (my/screen-layout)))
    (modify-frame-parameters (window-frame (get-buffer-window)) (my/align-right-frame-alist layout))))

(defun my/reset-frame-width ()
  "Reset the current frame width to function `my/cols'."
  (interactive)
  (let ((layout (my/screen-layout)))
    (set-frame-width (window-frame (get-buffer-window)) (my/cols layout))))

(defun my/share-screen-font-size (&optional arg)
  "Set font scaling to ARG when sharing screen.
ARG is an optional integer which defaults to 2."
  (interactive "P")
  (text-scale-set (or arg 2)))

(defun my/normal-screen-font-size ()
  "Remove any font scaling."
  (interactive)
  (text-scale-set 0))

(use-package ace-window
  :commands (aw-window-list aw-switch-to-window aw-select aw-flip-window ace-display-buffer ace-window)
  :defines (aw-dispatch-always)
  :config
  (setq aw-make-frame-char ?n))

(defun my/aw-make-frame ()
  "Make a new frame using layout settings for the current display.
The first frame always takes on `initial-frame-alist', and subsequent frames
use `default-frame-alist' by default. If there are already two frames active
then subsequent ones will be at `my/align-right-frame-alist' which aligns with
the right-edge of the screen, but may overlap with the middle frame."
  (let ((num-frames (length (visible-frame-list))))
    (if (< num-frames 2)
        (make-frame)
      (make-frame (my/align-right-frame-alist (my/screen-layout))))))

(advice-add 'aw-make-frame :override #'my/aw-make-frame)

(defun my/ace-window-always-dispatch ()
  "Invoke `ace-window' after setting `aw-dispatch-always' to T.
When `aw-dispatch-always' is nil, `ace-window' does not invoke
its dispatching mechanism if there are 2 or fewer windows. This
command guarantees that dispatching will always happen."
  (interactive)
  (let ((current-aw-dispatch-always aw-dispatch-always))
    (unwind-protect
        (let ((aw-dispatch-always t))
          (call-interactively #'ace-window))
      (setq aw-dispatch-always current-aw-dispatch-always))))

(use-package window
  :init
  (let ((window-parameters '(window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))))
    (message "%s" window-parameters)
    (setq switch-to-buffer-in-dedicated-window 'pop
          switch-to-buffer-obey-display-actions t
          window-resize-pixelwise t
          window-sides-slots '(0 0 3 1)
          display-buffer-base-action '((display-buffer-reuse-window ace-display-buffer))
          display-buffer-alist `(("\\*help\\[R" (display-buffer-reuse-mode-window ace-display-buffer) (reusable-frames . nil))
                                 ("\\*R" nil (reusable-frames . nil))
                                 ,(cons "\\*helm" display-buffer-fallback-action)
                                 ;; Show log buffer in something other than the current window
                                 ("magit-log" nil (inhibit-same-window . t))
                                 ("magit-diff:" nil (inhibit-same-window . t))))))

(when (display-graphic-p)
  (use-package nerd-icons)

  (use-package nerd-icons-completion
    :after (marginalia)
    :commands (nerd-icons-completion-mode nerd-icons-completion-marginalia-setup)
    :hook ((after-init . nerd-icons-completion-mode)
           (marginalia-mode nerd-icons-completion-marginalia-setup)))

  (use-package mood-line
    :commands (mood-line-mode)
    :hook
    (after-init . mood-line-mode)))

(if my/is-terminal
    (progn
      (set-face-background 'default "undefined")
      (when my/is-linux
        ;; Undo the mapping for ESC [ so it does not take over defined xterm sequences
        (define-key (current-global-map) (kbd "M-[") nil)
        (defvar arrow-keys-map (make-sparse-keymap) "Keymap for arrow keys")
        (define-key esc-map "O" arrow-keys-map)
        (define-key arrow-keys-map "A" #'previous-line)
        (define-key arrow-keys-map "B" #'next-line)
        (define-key arrow-keys-map "C" #'forward-char)
        (define-key arrow-keys-map "D" #'backward-char)))
  (use-package eldoc-box)
  (when my/is-macosx
    (custom-set-variables
     '(insert-directory-program "gls")
     '(frame-resize-pixelwise t)
     '(mac-command-modifier 'meta)
     '(mac-option-modifier 'alt)
     '(mac-right-command-modifier 'super)
     '(mac-right-control-modifier 'hyper))))

(provide 'my-display)
;;; my-display.el ends here
