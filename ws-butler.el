;;; ws-butler.el --- Unobtrusively remove trailing whitespace.

;; this file is not part of Emacs

;; Copyright (C) 2013 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: unobtrusively remove trailing whitespace
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Sat Jan  5 16:49:23 2013 (+0800)
;; Version: 0.2
;; Last-Updated:
;;           By:
;; URL: https://github.com/lewang/ws-butler
;; Keywords:
;; Compatibility: Emacs 24

;;; Installation:

;;
;; To enable for all ruby-mode buffers, add to .emacs.el:
;;
;;      (require 'ws-butler)
;;      (add-hook 'ruby-mode-hook 'ws-butler-mode)
;;

;;; Commentary:

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'hilit-chg))

(defgroup ws-butler nil
  "Unobtrusively whitespace deletion like a butler."
  :group 'convenience)

(defcustom ws-butler-keep-whitespace-before-point
  t
  "Keep whitespace at current point after save.

This is particularly to preserve indentation.

N.B. The spaces are removed in the file on disk.  So in a sense
only the \"virtual\" space is preserved in the buffer."
  :type 'boolean
  :group 'ws-butler)


(defvar ws-butler-saved)

(defmacro ws-butler-with-save (&rest forms)
  "Run FORMS with restriction and excursion saved once."
  (declare (debug (body)))
  `(if (and (boundp 'ws-butler-saved)
            ws-butler-saved)
       (progn
         ,@forms)
     (let ((ws-butler-saved t))
       (save-excursion
         (save-restriction
           ,@forms)))))

(defun ws-butler-trim-eob-lines ()
  "Trim lines at EOB in efficient manner.
Also see `require-final-newline'."
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
     (when (looking-at "\n\\(\n\\|\\'\\)")
       (forward-char 1)))
   (when require-final-newline
     (unless (bolp)
       (insert "\n")))
   (when (looking-at "\n+")
     (replace-match ""))))

(defun ws-butler-maybe-trim-eob-lines (last-modified-pos)
  "Delete extra newlines at end of buffer if LAST-MODIFIED-POS is in the patch of excess newlines."
  (interactive (list nil))
  (unless buffer-read-only
    (unless last-modified-pos
      (ws-butler-map-changes
       (lambda (_prop beg end)
         (setq last-modified-pos end))))
    ;; trim EOF newlines if required
    (when last-modified-pos
      (ws-butler-with-save
       (widen)
       (goto-char (point-max))
       (skip-chars-backward " \t\n\v")
       (let ((printable-point-max (point)))
         (when (>= last-modified-pos printable-point-max)
           (ws-butler-trim-eob-lines))))))
  ;; clean return code for hooks
  nil)

(defun ws-butler-clean-region (beg end)
  "Delete trailing blanks in region BEG END.

If `indent-tabs-mode' is nil, then tabs in indentation is
replaced by spaces."
  (interactive "*r")
  (ws-butler-with-save
   (narrow-to-region beg end)
   ;;  _much slower would be:       (replace-regexp "[ \t]+$" "")
   (goto-char (point-min))
   (while (not (eobp))
     ;; convert leading tabs to spaces
     (unless indent-tabs-mode
       (let ((eol (point-at-eol)))
         (skip-chars-forward " " (point-at-eol))
         (when (eq (char-after) ?\t )
           (untabify (point) (progn (skip-chars-forward " \t" (point-at-eol))
                                    (point))))))
     (end-of-line)
     (delete-horizontal-space)
     (forward-line 1)))
  ;; clean return code for hooks
  nil)


(defvar ws-butler-presave-coord nil
  "Saved list of (LINE COLUMN) used to restore point after saving.

This is the key to the virtual spaces preserving indentation mechanism.")
(make-variable-buffer-local 'ws-butler-presave-coord)

(defun ws-butler-map-changes (func &optional start-position end-position)
  "See `hilit-chg-map-changes'.

Call FUNC with each changed region (START-POSITION END-POSITION).

This simply uses an end marker since we are modifying the buffer
in place."

  (let ((start (or start-position (point-min)))
        (limit (copy-marker (or end-position (point-max))))
        prop end)
    (while (and start (< start limit))
      (setq prop (get-text-property start 'hilit-chg))
      (setq end (text-property-not-all start limit 'hilit-chg prop))
      (if prop
          (funcall func prop start (or end limit)))
      (setq start end))
    (set-marker limit nil)))

(defun ws-butler-before-save ()
  "Trim white space before save.

Setting `ws-butler-keep-whitespace-before-point' will also
ensure point doesn't jump due to white space trimming."
  ;; save data to restore later
  (when ws-butler-keep-whitespace-before-point
    (ws-butler-with-save
     (widen)
     (setq ws-butler-presave-coord (list
                                    (line-number-at-pos (point))
                                    (current-column)))))
  (let (last-end)
    (ws-butler-map-changes
     (lambda (_prop beg end)
       (save-excursion
         (setq beg (progn (goto-char beg)
                          (point-at-bol))
               ;; Subtract one from end to overcome Emacs bug #17784, since we
               ;; always expand to end of line anyway, this should be OK.
               end (progn (goto-char (1- end))
                          (point-at-eol))))
       (ws-butler-clean-region beg end)
       (setq last-end end)))
    (ws-butler-maybe-trim-eob-lines last-end)))

(defun ws-butler-after-save ()
  "Restore trimmed whitespace before point."

  ;; reset text properties
  (highlight-changes-mode 0)
  (highlight-changes-mode 1)
  ;; go to saved line+col
  (when ws-butler-presave-coord
    (let (remaining-lines)
      (ws-butler-with-save
       (widen)
       (goto-char (point-min))
       (setq remaining-lines (forward-line (1- (car ws-butler-presave-coord)))))
      (unless (eq remaining-lines 0)
        (insert (make-string remaining-lines ?\n))))
    (move-to-column (cadr ws-butler-presave-coord) t)
    (set-buffer-modified-p nil)))

(defun ws-butler-before-revert ()
  "Clear `ws-butler-presave-coord'."
  (setq ws-butler-presave-coord nil))

;;;###autoload
(define-minor-mode ws-butler-mode
  "White space cleanup mode implemented on top of `highlight-changes-mode'.

With this mode in operation, it's not possible to rotate changes,
etc.

Change visibility can be toggled with
`highlight-changes-visible-mode', but changes get reset on every
save."
  :lighter " wb"
  :group 'ws-butler
  (if ws-butler-mode
      (progn
        (require 'hilit-chg)
        (setq highlight-changes-visibility-initial-state nil)
        (highlight-changes-mode 1)
        (add-hook 'before-save-hook 'ws-butler-before-save t t)
        (add-hook 'after-save-hook 'ws-butler-after-save t t)
        (add-hook 'before-revert-hook 'ws-butler-before-revert t t)
        (add-hook 'after-revert-hook 'ws-butler-after-save t t)
        (add-hook 'edit-server-done-hook 'ws-butler-before-save t t))
    (highlight-changes-mode 0)
    (remove-hook 'before-save-hook 'ws-butler-before-save t)
    (remove-hook 'after-save-hook 'ws-butler-after-save t)
    (remove-hook 'before-revert-hook 'ws-butler-before-revert t)
    (remove-hook 'after-revert-hook 'ws-butler-after-save t)
    (remove-hook 'edit-server-done-hook 'ws-butler-before-save t)))

;;;###autoload
(define-globalized-minor-mode ws-butler-global-mode ws-butler-mode ws-butler-mode)

(provide 'ws-butler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ws-butler.el ends here
