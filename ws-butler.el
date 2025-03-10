;;; ws-butler.el --- Unobtrusively remove trailing whitespace  -*- lexical-binding:t -*-

;; Copyright (C) 2012-2016  Le Wang <l26wang@gmail.com>
;; Copyright (C) 2025  Sean Whitton <spwhitton@spwhitton.name>

;; Author: Le Wang <l26wang@gmail.com>
;; Maintainer: Sean Whitton <spwhitton@spwhitton.name>
;; Package-Requires: ((emacs "24.1"))
;; Version: 1.3
;; URL: https://elpa.nongnu.org/nongnu/ws-butler.html
;; Keywords: text

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To enable for, e.g., all ruby-mode buffers, add to your init.el:
;;
;;      (require 'ws-butler)
;;      (add-hook 'ruby-mode-hook 'ws-butler-mode)
;;

;;; News:

;; Ver 1.3 2025/03/10 Sean Whitton
;;     Replace a use of `always' to retain compatibility with older Emacs.
;;     Thanks to Chris Rayner for the reporting the problem.
;;
;; Ver 1.2 2025/02/25 Sean Whitton
;;     When `special-mode' is in `ws-butler-global-exempt-modes', also check
;;     whether a mode has a `mode-class' of `special', and don't activate
;;     `ws-butler-mode' if it does.
;;     Remove entries from `ws-butler-global-exempt-modes' that the preceding
;;     changes renders redundant.
;;     Exempt `org-agenda-mode' from `ws-butler-global-mode' by default.
;;
;; Ver 1.1 2025/02/21 Sean Whitton
;;     Exempt `compilation-mode' from `ws-butler-global-mode' by default.
;;     Clarify docstring of `ws-butler-global-exempt-modes'.
;;
;; Ver 1.0 2025/02/18 Sean Whitton
;;     Take over maintenance; maintain out of nongnu-elpa.git.
;;     Bump to version 1.0: core functionality not expected to change.
;;     Move customisation group from `convenience' to `text'.
;;     Rewrite docstrings.
;;     Fix accidental change `point-at-bol'->`line-end-position'.
;;     Exempt `message-mode' from `ws-butler-global-mode' by default.
;;     Other tidying.

;;; Code:

(defgroup ws-butler nil
  "Unobtrusive whitespace deletion, like a butler."
  :group 'text)

(defcustom ws-butler-keep-whitespace-before-point t
  "If non-nil, restore to the buffer trimmed whitespace right before point.
The effect is that cleanup of whitespace right before point is performed
on only the visited file on disk, while point in the buffer does not move.
This means that point does not jump when whitespace is trimmed."
  :type 'boolean
  :group 'ws-butler)

(defcustom ws-butler-convert-leading-tabs-or-spaces nil
  "If non-nil, apply `indent-tabs-mode' to leading indentation when saving.
If `indent-tabs-mode' is non-nil, convert leading indentation to use tabs,
like `tabify'.  Otherwise, replace all tabs with spaces, like `untabify'.
If `smart-tabs-mode' is enabled, these conversions are suppressed."
  :type 'boolean
  :group 'ws-butler)

(defcustom ws-butler-global-exempt-modes
  '(special-mode
    minibuffer-mode

    org-agenda-mode

    message-mode
    markdown-mode)
  "Trailing whitespace-significant major modes.
`ws-butler-global-mode' will not activate `ws-butler-mode' in these modes,
or in their derivatives.
If this list contains `special-mode', then in addition, `ws-butler-mode' will
not activate in all modes with a `mode-class' of `special'."
  :type '(repeat (symbol :tag "Major mode"))
  :group 'ws-butler)

(defcustom ws-butler-trim-predicate
  (lambda (_beg _end) t)
  "Function to exclude regions from whitespace trimming.
Called with two arguments delimiting a region of the current buffer.
If the function returns non-nil, trailing whitespace in that region will be
trimmed.  The default is to trim everywhere."
  :type 'function
  :group 'ws-butler)

(defvar ws-butler-saved)

(defmacro ws-butler-with-save (&rest forms)
  "Like `save-excursion' and `save-restriction', but only once.
If within the scope of another use of this macro, just evaluate FORMS."
  (declare (debug (body)))
  `(if (bound-and-true-p ws-butler-saved)
       (progn
         ,@forms)
     (let ((ws-butler-saved t))
       (save-excursion
         (save-restriction
           ,@forms)))))

(defun ws-butler-trim-eob-lines ()
  "Efficiently trim blank lines at end-of-buffer.
Affected by `require-final-newline', which see."
  (ws-butler-with-save
   (widen)
   ;; we need to clean up multiple blank lines at EOF to just one.  Or if
   ;; there is no blank line and there needs one, we add it.
   (goto-char (point-max))
   (skip-chars-backward " \t\n\v")
   (let ((saved-point (point)))
     (ws-butler-clean-region saved-point (point-max))
     (goto-char saved-point)
     ;; we try to make as few buffer modifications as possible
     ;;
     ;; We refuse to remove final-newline regardless of the value of
     ;; `require-final-newline'
     (when (looking-at-p "\n\\(?:\n\\|\\'\\)")
       (forward-char 1)))
   (when require-final-newline
     (unless (bolp)
       (insert "\n")))
   (when (looking-at "\n+")
     (replace-match ""))))

(defun ws-butler-maybe-trim-eob-lines (last-modified-pos)
  "Trim empty lines at end-of-buffer if LAST-MODIFIED-POS is within them."
  (interactive (list nil))
  (unless buffer-read-only
    (unless last-modified-pos
      (ws-butler-map-changes
       (lambda (_prop _beg end)
         (setq last-modified-pos end))))
    ;; trim EOF newlines if required
    (when last-modified-pos
      (ws-butler-with-save
       (widen)
       (goto-char (point-max))
       (skip-chars-backward " \t\n\v")
       (let ((printable-point-max (point)))
         (when (and (funcall ws-butler-trim-predicate
			     printable-point-max (point-max))
                  (>= last-modified-pos printable-point-max))
           (ws-butler-trim-eob-lines))))))
  ;; clean return code for hooks
  nil)

(defun ws-butler-clean-region (beg end)
  "Delete trailing whitespace in the region delimited by BEG and END.
Respects `ws-butler-convert-leading-tabs-or-spaces', which see."
  (interactive "*r")
  (ws-butler-with-save
   (narrow-to-region beg end)
   ;;  _much slower would be:       (replace-regexp "[ \t]+$" "")
   (goto-char (point-min))
   (while (not (eobp))
     (when (and ws-butler-convert-leading-tabs-or-spaces
                (not (bound-and-true-p smart-tabs-mode)))
       ;; convert leading tabs to spaces or v.v.
       (let ((eol (line-end-position)))
         (if indent-tabs-mode
             (progn
               (skip-chars-forward "\t" eol)
               (when (eq (char-after) ?\s)
                 (tabify (point) (progn (skip-chars-forward "\s\t" eol)
                                        (point)))))
           (skip-chars-forward " " eol)
           (when (eq (char-after) ?\t)
             (untabify (point) (progn (skip-chars-forward "\s\t" eol)
                                      (point)))))))
     (end-of-line)
     (delete-horizontal-space)
     (forward-line 1)))
  ;; clean return code for hooks
  nil)


(defvar ws-butler-presave-coord nil
  "Saved list of (LINE COLUMN) used to restore point after saving.

This is the key to the virtual spaces preserving indentation mechanism.")
(make-variable-buffer-local 'ws-butler-presave-coord)

;; Call FUNC with each changed region (START-POSITION END-POSITION).
;; This simply uses an end marker since we are modifying the buffer in place.
;; See also `hilit-chg-map-changes'.
(defun ws-butler-map-changes (func &optional start-position end-position)
  (let ((start (or start-position (point-min)))
        (limit (copy-marker (or end-position (point-max))))
        prop end)
    (while (and start (< start limit))
      (setq prop (get-text-property start 'ws-butler-chg))
      (setq end (text-property-not-all start limit 'ws-butler-chg prop))
      (if prop
          (funcall func prop start (or end limit)))
      (setq start end))
    (set-marker limit nil)))

(defun ws-butler-before-save ()
  "Trim white space before save.
Respects `ws-butler-keep-whitespace-before-point', which see."
  ;; save data to restore later
  (when ws-butler-keep-whitespace-before-point
    (ws-butler-with-save
     (widen)
     (setq ws-butler-presave-coord (list (line-number-at-pos (point))
					 (current-column)))))
  (let (last-end)
    (ws-butler-map-changes
     (lambda (_prop beg end)
       (save-excursion
         (setq beg (progn (goto-char beg)
                          (line-beginning-position))
               ;; Subtract one from end to overcome Emacs bug #17784, since we
               ;; always expand to end of line anyway, this should be OK.
               end (progn (goto-char (1- end))
                          (line-end-position))))
       (when (funcall ws-butler-trim-predicate beg end)
         (ws-butler-clean-region beg end))
       (setq last-end end)))
    (ws-butler-maybe-trim-eob-lines last-end)))

(defun ws-butler-clear-properties ()
  "Clear all `ws-butler-mode' text properties in the buffer."
  (with-silent-modifications
    (ws-butler-map-changes
     (lambda (_prop start end)
       (remove-list-of-text-properties start end '(ws-butler-chg))))))

(defun ws-butler-after-change (beg end length-before)
  "Update ws-butler text properties.
The arguments are as to members of `after-change-functions', which see."
  (let ((type (if (and (= beg end) (> length-before 0))
                  'delete
                'chg)))
    (if undo-in-progress
        ;; add back deleted text during undo
        (if (and (zerop length-before)
               (> end beg)
               (eq (get-text-property end 'ws-butler-chg) 'delete))
            (remove-list-of-text-properties end (1+ end) '(ws-butler-chg)))
      (with-silent-modifications
        (when (eq type 'delete)
          (setq end (min (+ end 1) (point-max))))
        (put-text-property beg end 'ws-butler-chg type)))))

(defun ws-butler-after-save ()
  "Restore trimmed whitespace before point."
  (ws-butler-clear-properties)
  ;; go to saved line+col
  (when ws-butler-presave-coord
    (let (remaining-lines)
      (ws-butler-with-save
       (widen)
       (goto-char (point-min))
       (setq remaining-lines
	     (forward-line (1- (car ws-butler-presave-coord)))))
      (unless (eq remaining-lines 0)
        (insert (make-string remaining-lines ?\n))))
    (move-to-column (cadr ws-butler-presave-coord) t)
    (set-buffer-modified-p nil)))

(defun ws-butler-before-revert ()
  "Clear `ws-butler-presave-coord'."
  (setq ws-butler-presave-coord nil))

;;;###autoload
(define-minor-mode ws-butler-mode
  "Whitespace cleanup without obtrusive whitespace removal.
Whitespaces at end-of-line and end-of-buffer are trimmed upon save, but
only for lines modified by you."
  :lighter " wb"
  :group 'ws-butler
  (if ws-butler-mode
      (progn
        (add-hook 'after-change-functions #'ws-butler-after-change t t)
        (add-hook 'before-save-hook #'ws-butler-before-save t t)
        (add-hook 'after-save-hook #'ws-butler-after-save t t)
        (add-hook 'before-revert-hook #'ws-butler-before-revert t t)
        (add-hook 'after-revert-hook #'ws-butler-after-save t t)
        (add-hook 'edit-server-done-hook #'ws-butler-before-save t t))
    (remove-hook 'after-change-functions #'ws-butler-after-change t)
    (remove-hook 'before-save-hook #'ws-butler-before-save t)
    (remove-hook 'after-save-hook #'ws-butler-after-save t)
    (remove-hook 'before-revert-hook #'ws-butler-before-revert t)
    (remove-hook 'after-revert-hook #'ws-butler-after-save t)
    (remove-hook 'edit-server-done-hook #'ws-butler-before-save t)))

;; It would be better to use a `:predicate' parameter to
;; `define-globalized-minor-mode', and mark `ws-butler-global-exempt-modes'
;; obsolete.  We could probably still honour a user's custom
;; `ws-butler-global-exempt-modes', if it was set, in this TURN-ON function.
;; Here is an example of a useful custom, user-specified predicate:
;;
;;     '((not markdown-mode
;;            message-mode
;;            lisp-interaction-mode)
;;       prog-mode conf-mode text-mode)
;;
;; However, this would mean bumping our minimum required Emacs version to
;; 28.1.  For a package like this one, I think it is too soon for that.
;;
;; We would also want to retain the special handling of `special-mode'.
(defun ws-butler--global-mode-turn-on ()
  "Enable `ws-butler-mode' unless current major mode is exempt."
  (unless (or (and (memq 'special-mode ws-butler-global-exempt-modes)
		   (eq (get major-mode 'mode-class) 'special))
	      (apply #'derived-mode-p ws-butler-global-exempt-modes))
    (ws-butler-mode 1)))

;;;###autoload
(define-globalized-minor-mode ws-butler-global-mode
  ws-butler-mode ws-butler--global-mode-turn-on)

(provide 'ws-butler)

;;; ws-butler.el ends here
