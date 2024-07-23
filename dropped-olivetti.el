;;; dropped-olivetti.el --- Minor mode for a nice writing environment -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2023  Paul W. Rankin
;; Copyright (c) 2024       Martin Edström <meedstrom91@gmail.com>

;; Author: Paul W. Rankin <hello@paulwrankin.com>
;; Keywords: wp, text
;; Version: 2.0.5
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/rnkn/dropped-olivetti

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'fringe)

(defgroup dropped-olivetti ()
  "Minor mode for a nice writing environment."
  :prefix "dropped-olivetti-"
  :group 'text)


;;; Internal Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'lisp-mnt)
  (defconst dropped-olivetti-version
    (lm-version load-file-name)))

(defvar dropped-olivetti--split-window-preferred-function
  nil
  "Value of `split-window-preferred-function' at initialization.")

(defvar dropped-olivetti--face-remap
  nil
  "Saved cookie from `face-remap-add-relative' at initialization.")


;;; Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom dropped-olivetti-body-width
  81
  "Text body width to which to adjust relative margin width.
If an integer, set text body width to that integer in columns; if
a floating point between 0.0 and 1.0, set text body width to that
fraction of the total window width.

An integer is best if you want text body width to remain
constant, while a floating point is best if you want text body
width to change with window width.

The floating point can anything between 0.0 and 1.0 (exclusive),
but use a value between about 0.33 and 0.9 for best effect.

This option does not affect file contents."
  :type '(choice (const :tag "Value of fill-column + 2" nil)
                 (integer 72)
                 (float 0.5))
  :safe (lambda (value)
          (or (numberp value) (null value))))
;; (make-variable-buffer-local 'dropped-olivetti-body-width)

(defcustom dropped-olivetti-minimum-body-width
  40
  "Minimum width in columns of text body."
  :type 'integer
  :safe 'integerp)

(defcustom dropped-olivetti-lighter
  " Olv"
  "Mode-line indicator for `dropped-olivetti-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp)

(defcustom dropped-olivetti-recall-visual-line-mode-entry-state
  t
  "Recall the state of `visual-line-mode' upon exiting.
When non-nil, remember if `visual-line-mode' was enabled or not
upon activating `dropped-olivetti-mode' and restore that state upon
exiting."
  :type 'boolean
  :safe 'booleanp)

(defcustom dropped-olivetti-style
  nil
  "Window elements used to balance the text body.
Valid options are:

    nil         use margins (default)
    t           use fringes
    fancy       use both margins with fringes outside

n.b. Fringes are only available on a graphical window system and
will fall back to margins on console."
  :type '(choice (const :tag "Margins" nil)
                 (const :tag "Fringes" t)
                 (const :tag "Fringes and Margins" fancy))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (featurep 'dropped-olivetti)
           (dropped-olivetti-reset-all-windows))))

(defcustom dropped-olivetti-margin-width
  10
  "Width in columns of margin between text body and fringes.
Only has any effect when `dropped-olivetti-style' is set to `fancy'."
  :type '(choice (const :tag "None" nil)
                 (integer :tag "Columns" 10))
  :safe 'integerp
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (featurep 'dropped-olivetti)
           (dropped-olivetti-reset-all-windows))))

(defface dropped-olivetti-fringe
  '((t (:inherit fringe)))
  "Face for the fringes when `dropped-olivetti-style' is non-nil."
  :group 'dropped-olivetti)


;;; Set Windows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dropped-olivetti-scale-width (width)
  "Scale WIDTH in accordance with the face height.
For compatibility with `text-scale-mode', if
`face-remapping-alist' includes a :height property on the default
face, scale WIDTH by that factor if it is a fraction, by (height/100)
if it is an integer, and otherwise return WIDTH."
  (let ((height (plist-get (cadr (assq 'default face-remapping-alist)) :height)))
    (when (integerp height)
      (setq height (/ height 100.0)))
    (round (* width (or height 1)))))

(defun dropped-olivetti-normalize-width (width window)
  "Parse WIDTH to a safe pixel value for `dropped-olivetti-body-width' for WINDOW."
  (let ((char-width (frame-char-width (window-frame window)))
        (window-width-pix (window-body-width window t))
        min-width-pix)
    (setq min-width-pix (* char-width
                           (+ dropped-olivetti-minimum-body-width
                              (% dropped-olivetti-minimum-body-width 2))))
    (dropped-olivetti-scale-width
     (if (floatp width)
         (floor (max min-width-pix (* window-width-pix (min width 1.0))))
       (max min-width-pix (min (* width char-width) window-width-pix))))))

(defun dropped-olivetti-reset-window (window)
  "Remove Dropped-Olivetti's parameters and margins from WINDOW."
  (when (eq (window-parameter window 'split-window) 'dropped-olivetti-split-window)
    (set-window-parameter window 'split-window nil))
  (if (consp fringe-mode)
      (set-window-fringes window (car fringe-mode) (cdr fringe-mode))
    (set-window-fringes window fringe-mode fringe-mode))
  (set-window-margins window nil))

(defun dropped-olivetti-reset-all-windows ()
  "Call `dropped-olivetti-reset-window' on all windows."
  (walk-windows #'dropped-olivetti-reset-window nil t))

(defun dropped-olivetti-split-window (&optional window size side pixelwise)
  "Call `split-window' after resetting WINDOW.
Pass SIZE, SIDE and PIXELWISE unchanged."
  (dropped-olivetti-reset-all-windows)
  (split-window window size side pixelwise))

(defun dropped-olivetti-split-window-sensibly (&optional window)
  "Like `dropped-olivetti-split-window' but call `split-window-sensibly'.
Pass WINDOW unchanged."
  (dropped-olivetti-reset-all-windows)
  (funcall dropped-olivetti--split-window-preferred-function window))

(defun dropped-olivetti-set-window (window-or-frame)
  "Balance window margins displaying current buffer.
If WINDOW-OR-FRAME is a frame, cycle through windows displaying
current buffer in that frame, otherwise only work on the selected
window."
  (if (framep window-or-frame)
      (with-selected-frame window-or-frame
        (walk-windows #'dropped-olivetti-set-window))
    (with-selected-window window-or-frame
      (unless (minibufferp)
        (dropped-olivetti-reset-window window-or-frame)
        (let ((char-width-pix   (frame-char-width (window-frame window-or-frame)))
              (window-width-pix (window-body-width window-or-frame t))
              (safe-width-pix   (dropped-olivetti-normalize-width
                                 dropped-olivetti-body-width window-or-frame)))
          ;; Handle possible display of fringes
          (when (and window-system dropped-olivetti-style)
            (let ((fringe-total (- (window-pixel-width window-or-frame)
                                   safe-width-pix))
                  fringe)
              ;; Account for fancy display
              (when (eq dropped-olivetti-style 'fancy)
                (setq fringe-total
                      (- fringe-total
                         (* dropped-olivetti-margin-width char-width-pix 2))))
              ;; Calculate a single fringe width
              (setq fringe (max (round (/ fringe-total 2.0)) 0))
              ;; Set the fringes
              (set-window-fringes window-or-frame fringe fringe t)))
          ;; Calculate margins widths as body pixel width less fringes
          (let ((fringes (window-fringes window-or-frame))
                (margin-total-pix (/ (- window-width-pix safe-width-pix) 2.0))
                left-margin right-margin)
            ;; Convert to character cell columns
            (setq left-margin  (max (round (/ (- margin-total-pix
                                                 (car fringes))
                                              char-width-pix))
                                    0)
                  right-margin (max (round (/ (- margin-total-pix
                                                 (cadr fringes))
                                              char-width-pix))
                                    0))
            ;; Finally set the margins
            (set-window-margins window-or-frame left-margin right-margin)))
        ;; Set remaining window parameters
        (set-window-parameter window-or-frame 'split-window
                              'dropped-olivetti-split-window)))))

(defun dropped-olivetti-set-buffer-windows ()
  "Balance window margins in all windows displaying current buffer.
Cycle through all windows in all visible frames displaying the
current buffer, and call `dropped-olivetti-set-window'."
  (mapc #'dropped-olivetti-set-window (get-buffer-window-list nil nil 'visible)))



;;; Mode Definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'face-remap)

;;;###autoload
(define-minor-mode dropped-olivetti-mode
  "Dropped-Olivetti provides a nice writing environment.
Window margins are set to relative widths to accomodate a text
body width set with `dropped-olivetti-body-width'."
  :global t
  (if dropped-olivetti-mode
      (progn
        (add-hook 'window-size-change-functions
                  #'dropped-olivetti-set-window t)
        ;; (add-hook 'change-major-mode-hook
        ;; #'dropped-olivetti-reset-all-windows nil)
        (add-hook 'text-scale-mode-hook
                  #'dropped-olivetti-set-buffer-windows t)
        (unless (bound-and-true-p dropped-olivetti--split-window-preferred-function)
          (setq dropped-olivetti--split-window-preferred-function
                split-window-preferred-function))
        (setq split-window-preferred-function
              #'dropped-olivetti-split-window-sensibly)
        (setq dropped-olivetti--face-remap
              (face-remap-add-relative 'fringe 'dropped-olivetti-fringe))
        (walk-windows #'dropped-olivetti-set-window nil t))
    (remove-hook 'window-configuration-change-hook
                 #'dropped-olivetti-set-buffer-windows )
    (remove-hook 'window-size-change-functions
                 #'dropped-olivetti-set-window )
    (remove-hook 'text-scale-mode-hook
                 #'dropped-olivetti-set-window )
    (dropped-olivetti-set-buffer-windows)
    (set-window-margins nil left-margin-width right-margin-width)
    (setq split-window-preferred-function
          dropped-olivetti--split-window-preferred-function)
    (when dropped-olivetti--face-remap
      (face-remap-remove-relative dropped-olivetti--face-remap))
    (mapc #'kill-local-variable '(dropped-olivetti--face-remap
                                  dropped-olivetti--split-window-preferred-function))))



(provide 'dropped-olivetti)

;;; dropped-olivetti.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
