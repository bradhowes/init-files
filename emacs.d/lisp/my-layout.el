;;; package -- my-layout -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Load personal layout definitions.
;;; Code:

(require 'my-constants)
(require 'my-customizations)

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

(defun my/frame-left (layout display)
  "The `left' position on LAYOUT and DISPLAY for a `left' frame.
The position value will place the frame flush with the left-hand side of the
display. This is used in a frame alist, in particular the `initial-frame-alist'
configuration."
  (declare (side-effect-free t))
  ;; Use an external monitor if there is one.
  (if (memq layout '(my/screen-laptop-4k my/screen-laptop-4k-4k))
      (+ my/laptop-screen-width (* display  my/4k-screen-width))
    (if my/is-x-windows-on-win 0 0)))

(defun my/frame-center (layout display)
  "The `left' position on LAYOUT and DISPLAY for a `center' frame.
The position value will place the frame such that it does not overlap with
another frame in the `left' position.
This is used in a frame alist, in particular the `default-frame-alist'
configuration."
  (declare (side-effect-free t))
  (+ (my/frame-left layout display) (my/frame-pixel-width layout)))

(defun my/frame-right (layout display)
  "The `left' position on LAYOUT and DISPLAY for a `right' frame.
The position value will place the frame flush with the right-hand side
of the display. This is not used in any particular `*-frame-alist' but
it is used by custom commands."
  (declare (side-effect-free t))
  (- (+ (my/frame-left layout display)
        (if (eq layout my/screen-laptop)
            my/laptop-screen-width
          my/4k-screen-width))
     (my/frame-pixel-width layout)))

(defun my/frame-top ()
  "The top of the display area.
NOTE: this assumes that the laptop display, if present,
is on the left of any monitors."
  (declare (side-effect-free t))
  (let* ((settings (display-monitor-attributes-list))
         (top (nth 2 (if (eq (length settings) 1)
                         (car (car settings))
                       (car (car (cdr settings))))))
         (offset (if my/is-x-windows-on-win 30 0)))
    (list '+ (+ offset top))))

(defun my/frame-left-alist (layout display)
  "Make alist to use for the `initial' frame on LAYOUT and DISPLAY.
Frame's left side is flush with the left side of the DISPLAY."
  (declare (side-effect-free t))
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-left layout display))))

(defun my/frame-center-alist (layout display)
  "Make alist to use for the `default' frame on LAYOUT and DISPLAY.
Frame's left side is next to the right side of the initial frame."
  (declare (side-effect-free t))
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-center layout display))))

(defun my/frame-right-alist (layout display)
  "Make alist to use for the `right' frame on LAYOUT and DISPLAY.
Frame's right side is flush with the right side of the main display."
  (declare (side-effect-free t))
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-right layout display))))

(defun my/update-screen-frame-alists (layout)
  "Update frame alists for current LAYOUT and DISPLAY."
  (modify-all-frames-parameters (my/frame-center-alist layout my/screen-4k-pick))
  (setq initial-frame-alist (my/frame-left-alist layout my/screen-4k-pick))
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

(add-hook 'after-init-hook #'my/screen-layout-changed)

(defun my/share-screen-font-size (&optional arg)
  "Set font scaling to ARG when sharing screen.
ARG is an optional integer which defaults to 2."
  (interactive "P")
  (text-scale-set (or arg 2)))

(defun my/normal-screen-font-size ()
  "Remove any font scaling."
  (interactive)
  (text-scale-set 0))

(defun my/frame-set-alist (frame-alist)
  "Apply FRAME-ALIST to current frame."
  (let ((frame (window-frame (get-buffer-window))))
    (modify-frame-parameters frame frame-alist)
    (redraw-frame frame)))

(defun my/frame-pos-display1-left ()
  "Reset frame size and position for left frame."
  (interactive)
  (my/frame-set-alist (my/frame-left-alist (my/screen-layout) my/4k-display-1)))

(defun my/frame-pos-display1-center ()
  "Reset frame size and position for right frame."
  (interactive)
  (my/frame-set-alist (my/frame-center-alist (my/screen-layout) my/4k-display-1)))

(defun my/frame-pos-display1-right ()
  "Reset frame size and position for alternative right position.
This will shift the frame to the left so that it does not overlap with any
frame that is abutting the right edge of the display."
  (interactive)
  (my/frame-set-alist (my/frame-right-alist (my/screen-layout) my/4k-display-1)))

(defun my/frame-pos-display2-left ()
  "Reset frame size and position for left frame."
  (interactive)
  (my/frame-set-alist (my/frame-left-alist (my/screen-layout) my/4k-display-2)))

(defun my/frame-pos-display2-center ()
  "Reset frame size and position for right frame."
  (interactive)
  (my/frame-set-alist (my/frame-center-alist (my/screen-layout) my/4k-display-2)))

(defun my/frame-pos-display2-right ()
  "Reset frame size and position for alternative right position.
This will shift the frame to the left so that it does not overlap with any
frame that is abutting the right edge of the display."
  (interactive)
  (my/frame-set-alist (my/frame-right-alist (my/screen-layout) my/4k-display-2)))

(defun my/frame-pos-left (&optional screen)
  "Reset frame size and position for left frame on SCREEN."
  (interactive "P")
  (if (eq 0 (or screen my/screen-4k-pick))
      (my/frame-pos-display1-left)
    (my/frame-pos-display2-left)))

(defun my/frame-pos-center (&optional screen)
  "Reset frame size and position for center frame on SCREEN."
  (interactive "P")
  (if (eq 0 (or screen my/screen-4k-pick))
      (my/frame-pos-display1-center)
    (my/frame-pos-display2-center)))

(defun my/frame-pos-right (&optional screen)
  "Reset frame size and position for right frame on SCREEN."
  (interactive "P")
  (if (eq 0 (or screen my/screen-4k-pick))
      (my/frame-pos-display1-right)
    (my/frame-pos-display2-right)))

;; (keymap-global-set "C-M-<f2>" #'my/reset-frame-alt-right)

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

(provide 'my-layout)

;;; my-layout.el ends here
