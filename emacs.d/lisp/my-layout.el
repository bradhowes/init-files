;;; package -- my-layout -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Load personal layout definitions.
;;; Code:

(require 'my-constants)
(require 'my-customizations)

(defconst my/layout--screens-laptop
  (intern "my/layout--screens-laptop")
  "Symbol to indicate the laptop display.")

(defconst my/layout--screens-4k
  (intern "my/layout--screens-4k")
  "Symbol to indicate display is 4K screen.")

(defconst my/layout--screens-laptop-4k
  (intern "my/layout--screens-laptop-4k")
  "Symbol to indicate display width is laptop and 1 4K screen.")

(defconst my/layout--screens-4k-4k
  (intern "my/layout--screens-4k-4k")
  "Symbol to indicate display width is 2 4K screens.")

(defconst my/layout--screens-laptop-4k-4k
  (intern "my/layout--screens-laptop-4k-4k")
  "Symbol to indicate display width is laptop and 2 4K screens.")

(defconst my/layout--screens-terminal
  (intern "my/layout--screens-terminal")
  "Symbol to indicate display is a terminal.")

(defconst my/layout--use-4k-display-1
  0
  "Symbol to indicate first 4K display.")

(defconst my/layout--use-4k-display-2
  1
  "Symbol to indicate second 4k display.")

(defconst my/layout--laptop-screen-width
  2056
  "MacBook Pro 16\" M1 screen width in pixels.")

(defconst my/layout--4k-screen-width
  3840
  "4K external display width in pixels.")

(defun my/layout--active-screens ()
  "Identify current screen layout.
Uses result from `display-pixel-width' to determine what monitors
there are.  Better would be to use `display-monitor-attributes-list'
like done in `my/frame-top'.

Returns one of the follow symbols based on width:

- `my/layout--laptop' -- only laptop screen
- `my/layout--4k' -- only 4K monitor.
- `my/layout--laptop-4k' -- laptop screen + 4K monitor.
- `my/layout--4k-4k' -- two 4K monitors.
- `my/layout--laptop-4k-4k' -- laptop screen + 2 4K monitors.
- `my/layout--terminal' -- unknown screen."
  (declare (side-effect-free t))
  (let* ((width (display-pixel-width nil))
         (value (cond
                 ((= width my/layout--laptop-screen-width)
                  my/layout--screens-laptop)
                 ((= width my/layout--4k-screen-width)
                  my/layout--screens-4k)
                 ((= width (+ my/layout--laptop-screen-width my/layout--4k-screen-width))
                  my/layout--screens-laptop-4k)
                 ((= width (* my/layout--4k-screen-width 2))
                  my/layout--screens-4k-4k)
                 ((= width (+ my/layout--laptop-screen-width (* 2 my/layout--4k-screen-width)))
                  my/layout--screens-laptop-4k-4k)
                 (t my/layout--screens-terminal))))
    (message "my/layout-active-screens: %s" value)
    value))

(defun my/layout--which-4k-display ()
  "Obtain the index of the 4K screen to use (zero-based).
Usually this is the value from `my/layout-default-display-4k' but it is
limited by the number of actual 4K displays that are present at the time
of the call."
  (declare (side-effect-free t))
  (let ((layout (my/layout--active-screens)))
    (min my/layout-default-display-4k
         (cond
          ((eq layout my/layout--screens-laptop-4k-4k) my/layout--use-4k-display-2)
          ((eq layout my/layout--screens-4k-4k) my/layout--use-4k-display-2)
          (t my/layout--use-4k-display-1)))))

(defun my/layout--is-laptop (layout)
  "T if LAYOUT is laptop-only."
  (declare (side-effect-free t))
  (eq layout my/layout--screens-laptop))

(defun my/layout--has-4k (layout)
  "T if LAYOUT is kind with at least one 4K area."
  (declare (side-effect-free t))
  (memq layout '(my/layout--screens-4k my/layout--screens-laptop-4k my/layout--screens-4k-4k my/layout--screens-laptop-4k-4k)))

(defun my/layout--rows (layout)
  "The number of rows to show in a frame shown on LAYOUT."
  (declare (side-effect-free t))
  (if (my/layout--has-4k layout)
      my/layout-rows-4k
    (if (my/layout--is-laptop layout)
        my/layout-rows-laptop
      my/layout-rows-terminal)))

(defun my/layout--cols (layout)
  "The number of columns to show in a frame shown on LAYOUT."
  (declare (side-effect-free t))
  (if (my/layout--has-4k layout)
      my/layout-cols-4k
    (if (my/layout--is-laptop layout)
        my/layout-cols-laptop
      my/layout-cols-terminal)))

(defun my/layout--frame-pixel-width (layout)
  "Width in pixels of a normal frame shown on LAYOUT.
These values are hard-coded based on current settings.
Probably a better way to figure this out."
  (declare (side-effect-free t))
  (if (my/layout--has-4k layout)
      my/layout-frame-pixel-width-4k
    my/layout-frame-pixel-width-laptop))

(defun my/layout--frame-left (layout display)
  "The `left' position on LAYOUT and DISPLAY for a `left' frame.
The position value will place the frame flush with the left-hand side of the
display. This is used in a frame alist, in particular the `initial-frame-alist'
configuration."
  (declare (side-effect-free t))
  ;; Use an external monitor if there is one.
  (if (memq layout '(my/layout--screens-laptop-4k my/layout--screens-laptop-4k-4k))
      (+ my/layout--laptop-screen-width (* display  my/layout--4k-screen-width))
    0))

(defun my/layout--frame-center (layout display)
  "The `left' position on LAYOUT and DISPLAY for a `center' frame.
The position value will place the frame such that it does not overlap with
another frame in the `left' position.
This is used in a frame alist, in particular the `default-frame-alist'
configuration."
  (declare (side-effect-free t))
  (+ (my/layout--frame-left layout display) (my/layout--frame-pixel-width layout)))

(defun my/layout--frame-right (layout display)
  "The `left' position on LAYOUT and DISPLAY for a `right' frame.
The position value will place the frame flush with the right-hand side
of the display. This is not used in any particular `*-frame-alist' but
it is used by custom commands."
  (declare (side-effect-free t))
  (- (+ (my/layout--frame-left layout display)
        (if (eq layout my/layout--screens-laptop)
            my/layout--laptop-screen-width
          my/layout--4k-screen-width))
     (my/layout--frame-pixel-width layout)))

(defun my/layout--frame-top ()
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

(defun my/layout--frame-left-alist (layout display)
  "Make alist to use for the `initial' frame on LAYOUT and DISPLAY.
Frame's left side is flush with the left side of the DISPLAY."
  (declare (side-effect-free t))
  (list (cons 'width (my/layout--cols layout))
        (cons 'height (my/layout--rows layout))
        (cons 'top (my/layout--frame-top))
        (cons 'left (my/layout--frame-left layout display))))

(defun my/layout--frame-center-alist (layout display)
  "Make alist to use for the `default' frame on LAYOUT and DISPLAY.
Frame's left side is next to the right side of the initial frame."
  (declare (side-effect-free t))
  (list (cons 'width (my/layout--cols layout))
        (cons 'height (my/layout--rows layout))
        (cons 'top (my/layout--frame-top))
        (cons 'left (my/layout--frame-center layout display))))

(defun my/layout--frame-right-alist (layout display)
  "Make alist to use for the `right' frame on LAYOUT and DISPLAY.
Frame's right side is flush with the right side of the main display."
  (declare (side-effect-free t))
  (list (cons 'width (my/layout--cols layout))
        (cons 'height (my/layout--rows layout))
        (cons 'top (my/layout--frame-top))
        (cons 'left (my/layout--frame-right layout display))))

(defun my/layout--update-screen-frame-alists (layout)
  "Update frame alists for current LAYOUT and DISPLAY."
  (modify-all-frames-parameters (my/layout--frame-center-alist layout (my/layout--which-4k-display)))
  (setq initial-frame-alist (my/layout--frame-left-alist layout (my/layout--which-4k-display)))
  (message "initial-frame: %s" initial-frame-alist)
  (message "default-frame: %s" default-frame-alist))

(defun my/layout--font-size (layout)
  "The font size to use based on the LAYOUT."
  (declare (side-effect-free t))
  (if (my/layout--has-4k layout) my/layout-font-size-4k my/layout-font-size-laptop))

(defun my/layout--setup-font (layout)
  "Install the desired font in the default face for LAYOUT."
  (set-face-attribute 'default nil :font (font-spec :family my/font-name :size (my/layout--font-size layout))))

(defun my/layout--screens-layout-changed ()
  "Recalculate values based on screen layout."
  (interactive)
  (let ((layout (my/layout--active-screens)))
    (message "screen layout: %s" layout)
    (my/layout--setup-font layout)
    (my/layout--update-screen-frame-alists layout)))

(defun my/layout-pick-default-display-4k (screen)
  "Set the 4K SCREEN to use to host future Emacs frames.
It does not affect existing frames."
  (interactive "NScreen:")
  (custom-set-variables (list 'my/layout-default-display-4k screen))
  (custom-save-all)
  (my/layout--screens-layout-changed))

(add-hook 'after-init-hook #'my/layout--screens-layout-changed)

(defun my/layout-share-screen-font-size (&optional arg)
  "Set font scaling to ARG when sharing screen.
ARG is an optional integer which defaults to 2."
  (interactive "P")
  (text-scale-set (or arg 2)))

(defun my/layout-normal-screen-font-size ()
  "Remove any font scaling."
  (interactive)
  (text-scale-set 0))

(defun my/layout--frame-set-alist (frame-alist)
  "Apply FRAME-ALIST to current frame."
  (let ((frame (window-frame (get-buffer-window))))
    (modify-frame-parameters frame frame-alist)
    (redraw-frame frame)))

(defun my/layout-frame-pos-display1-left ()
  "Reset frame size and position for left frame."
  (interactive)
  (my/layout--frame-set-alist (my/layout--frame-left-alist (my/layout--active-screens) my/layout--use-4k-display-1)))

(defun my/layout-frame-pos-display1-center ()
  "Reset frame size and position for right frame."
  (interactive)
  (my/layout--frame-set-alist (my/layout--frame-center-alist (my/layout--active-screens) my/layout--use-4k-display-1)))

(defun my/layout-frame-pos-display1-right ()
  "Reset frame size and position for alternative right position.
This will shift the frame to the left so that it does not overlap with any
frame that is abutting the right edge of the display."
  (interactive)
  (my/layout--frame-set-alist (my/layout--frame-right-alist (my/layout--active-screens) my/layout--use-4k-display-1)))

(defun my/layout-frame-pos-display2-left ()
  "Reset frame size and position for left frame."
  (interactive)
  (my/layout--frame-set-alist (my/layout--frame-left-alist (my/layout--active-screens) my/layout--use-4k-display-2)))

(defun my/layout-frame-pos-display2-center ()
  "Reset frame size and position for right frame."
  (interactive)
  (my/layout--frame-set-alist (my/layout--frame-center-alist (my/layout--active-screens) my/layout--use-4k-display-2)))

(defun my/layout-frame-pos-display2-right ()
  "Reset frame size and position for alternative right position.
This will shift the frame to the left so that it does not overlap with any
frame that is abutting the right edge of the display."
  (interactive)
  (my/layout--frame-set-alist (my/layout--frame-right-alist (my/layout--active-screens) my/layout--use-4k-display-2)))

(defun my/layout-frame-pos-left (&optional display)
  "Reset frame size and position for left frame on DISPLAY.
If there are multiple 4K displays, by default the display to use for the new
frame will be that found in `my/layout-default-display-4k'. A specific screen
can be chosen by providing a prefix value where 0 is the first display, and 1
is the second, etc."
  (interactive "P")
  (if (eq 0 (or display (my/layout--which-4k-display)))
      (my/layout-frame-pos-display1-left)
    (my/layout-frame-pos-display2-left)))

(defun my/layout-frame-pos-center (&optional display)
  "Reset frame size and position for center frame on DISPLAY.
If there are multiple 4K displays, by default the display to use for the new
frame will be that found in `my/layout-default-display-4k'. A specific screen
can be chosen by providing a prefix value where 0 is the first display, and 1
is the second, etc."
  (interactive "P")
  (if (eq 0 (or display (my/layout--which-4k-display)))
      (my/layout-frame-pos-display1-center)
    (my/layout-frame-pos-display2-center)))

(defun my/layout-frame-pos-right (&optional display)
  "Reset frame size and position for right frame on DISPLAY.
If there are multiple 4K displays, by default the display to use for the new
frame will be that found in `my/layout-default-display-4k'. A specific screen
can be chosen by providing a prefix value where 0 is the first display, and 1
is the second, etc."
  (interactive "P")
  (if (eq 0 (or display (my/layout--which-4k-display)))
      (my/layout-frame-pos-display1-right)
    (my/layout-frame-pos-display2-right)))

(defun my/layout-reset-frame-width ()
  "Reset the current frame width to function `my/cols'."
  (interactive)
  (let ((layout (my/layout--active-screens)))
    (set-frame-width (window-frame (get-buffer-window)) (my/layout--cols layout))))

(provide 'my-layout)

;;; my-layout.el ends here
