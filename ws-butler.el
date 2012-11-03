(defun ws-butler-trim-region (beg end)
  "Delete trailing blanks in region BEG END."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      ;;  _much slower would be:       (replace-regexp "[ \t]+$" "")
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (delete-horizontal-space)
        (forward-line 1))))
  nil)                                  ;for possible hook


(defvar ws-butler-saved-ws nil)
(make-variable-buffer-local 'ws-butler-saved-ws)

(defun ws-butler-map-map-changes (func &optional start-position end-position)
  "See `hilit-chg-map-changes'.  This simply uses an end marker
  since we are modifying the buffer in place."

  (let ((start (or start-position (point-min)))
        (limit (copy-marker (or end-position (point-max))))
        prop end)
    (while (and start (< start limit))
      (setq prop (get-text-property start 'hilit-chg))
      (setq end (text-property-not-all start limit 'hilit-chg prop))
      (if prop
          (funcall func prop start (or end limit)))
      (setq start end))))

(defun ws-butler-before-save ()
  "Trim white space before save.

This will also ensure point doesn't jump due to white space
trimming.  (i.e. keep whitespace after EOL text but before
point."
  (setq ws-butler-saved-ws nil)
  (ws-butler-map-map-changes
   (lambda (_prop beg end)
     (save-excursion
       (setq beg (progn (goto-char beg)
                        (point-at-bol))
             end (progn (goto-char end)
                        (point-at-eol))))
     (message (format "-----------point is %s" (point)))
     (when (and (>= (point) beg)
                (<= (point) end))
       (setq ws-butler-saved-ws (when (and (looking-at-p "\\s-*$")
                                           (looking-back "\\s-+" (line-beginning-position) t))
                                   (match-string 0))))
     (ws-butler-trim-region beg end))))

(defun ws-butler-after-save ()
  "Restore trimmed whitespace before point."

  ;; reset text properties
  (highlight-changes-mode 0)
  (highlight-changes-mode 1)
  (when ws-butler-saved-ws
    (insert ws-butler-saved-ws))
  (set-buffer-modified-p nil))

(defun ws-butler-before-revert ()
  "Clear `ws-butler-saved-ws'"
  (setq ws-butler-saved-ws nil))

(define-minor-mode ws-butler-mode
  "White space cleanup mode implemented on top of `highlight-changes-mode'.

With this mode in operation, it's not possible to rotate changes,
etc.

Change visibility can be toggled with
`highlight-changes-visible-mode', but changes get reset on every
save."
  :lighter " tr"
  :group 'ws-butler
  (if ws-butler-mode
      (progn
        (require 'hilit-chg)
        (setq highlight-changes-visibility-initial-state nil)
        (highlight-changes-mode 1)
        (add-hook 'before-save-hook 'ws-butler-before-save t t)
        (add-hook 'after-save-hook 'ws-butler-after-save t t)
        (add-hook 'after-revert-hook 'ws-butler-before-revert t t)
        (add-hook 'after-revert-hook 'ws-butler-after-save t t)
        (add-hook 'edit-server-done-hook 'ws-butler-before-save t t))
    (highlight-changes-mode 0)
    (remove-hook 'before-save-hook 'ws-butler-before-save t)
    (remove-hook 'after-save-hook 'ws-butler-after-save t)
    (remove-hook 'after-revert-hook 'ws-butler-before-revert t)
    (remove-hook 'after-revert-hook 'ws-butler-after-save t)
    (remove-hook 'edit-server-done-hook 'ws-butler-before-save t)))

