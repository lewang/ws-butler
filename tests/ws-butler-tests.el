
(require 'ert)

;; (require 'popup)

;; for "every" function
(require 'cl)
(load-file "ws-butler.el")

(defmacro ws-butler-test-with-test-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(let ((test-buffer-name "*ws-butler test*"))
     (save-excursion
       (when (get-buffer test-buffer-name)
         (kill-buffer test-buffer-name))
       (switch-to-buffer (get-buffer-create test-buffer-name))
       ,@body)))

(defmacro ws-butler-test-with-common-setup (&rest body)
  (declare (indent 0) (debug t))
  `(ws-butler-test-with-test-buffer
    ,@body))


(ert-deftest ws-butler-test-sanity ()
  "sanity check."
  (ws-butler-test-with-common-setup
    (insert "a b c")
    (execute-kbd-macro (read-kbd-macro "M-DEL"))
    (should (every #'identity (list 1 2 3)))
    (should (string-equal (buffer-string) "a b "))))

(ert-deftest ws-butler-test-trim-predicate ()
  "Tests `ws-butler-trim-predicate'."
  (ws-butler-test-with-common-setup
   (let ((ws-butler-trim-predicate (lambda (_beg _end) false)))
     (insert "a b c. \n")
     (ws-butler-before-save)
     (should (string-equal (buffer-string) "a b c. \n")))
   (let (ws-butler-trim-predicate)
     (erase-buffer)
     (insert "a b c. \n")
     (ws-butler-before-save)
     (should (string-equal (buffer-string) "a b c.\n")))))
