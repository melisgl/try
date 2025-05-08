;; -*- lexical-binding: t -*-

(defgroup mgl-try-faces nil
  "Faces in mgl-try buffers."
  :prefix "mgl-try-")

(defface mgl-try-abort-face
  '((t (:inherit error :bold t)))
  "Face for TRY:ABORT* event markers."
  :group 'mgl-try-faces)

(defface mgl-try-unexpected-failure-face
  '((t (:inherit error :bold t)))
  "Face for TRY:UNEXPECTED-FAILURE event markers."
  :group 'mgl-try-faces)

(defface mgl-try-unexpected-success-face
  '((t (:inherit warning :bold t)))
  "Face for TRY:UNEXPECTED-SUCCESS event markers."
  :group 'mgl-try-faces)

(defface mgl-try-skip-face
  '((t (:inherit warning)))
  "Face for TRY:SKIP event markers."
  :group 'mgl-try-faces)

(defface mgl-try-expected-failure-face
  '((t (:inherit warning)))
  "Face for TRY:EXPECTED-FAILURE event markers."
  :group 'mgl-try-faces)

(defface mgl-try-expected-success-face
  '((t (:inherit success)))
  "Face for TRY:EXPECTED-SUCCESS event markers."
  :group 'mgl-try-faces)

(defvar mgl-try-abort-regexp "^\\*+ ⊟")
(defvar mgl-try-unexpected-failure-regexp "^\\*+ ⊠")
(defvar mgl-try-unexpected-success-regexp "^\\*+ ⊡")
(defvar mgl-try-skip-regexp "^\\*+ -")
(defvar mgl-try-expected-failure-regexp "^\\*+ ×")
(defvar mgl-try-expected-success-regexp "^\\*+ ⋅")

(defvar mgl-try-history '()
  "History list of expressions read from the minibuffer.")

(defvar mgl-try-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") 'outline-cycle)
    (define-key map (kbd "C-P") 'outline-previous-visible-heading)
    (define-key map (kbd "C-n") 'outline-next-visible-heading)
    (define-key map (kbd "U") 'outline-up-heading)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "p") 'mgl-try-previous-unexpected)
    (define-key map (kbd "n") 'mgl-try-next-unexpected)
    (define-key map (kbd "P") 'mgl-try-previous-not-expected-success)
    (define-key map (kbd "N") 'mgl-try-next-not-expected-success)
    (define-key map (kbd "t") 'mgl-try)
    (define-key map (kbd "r") 'mgl-try-rerun-!)
    (define-key map (kbd "R") 'mgl-try-rerun-!-all)
    map))

(define-minor-mode mgl-try-mode
  "\\<mgl-try-mode-map>\
  mgl-try: a minor mode to interact with the Common Lisp TRY library.
  \\{mgl-try-mode-map}"
  :lighter " Try"
  :keymap mgl-try-mode-map)

(defun mgl-try-read-from-minibuffer (prompt &optional initial-value
                                            default-value history)
  (let* ((minibuffer-setup-hook (slime-minibuffer-setup-hook))
         (prompt (if default-value
                     (concat prompt " (default: " default-value "): ")
                   (concat prompt ": ")))
         (string (read-from-minibuffer prompt initial-value
                                       slime-minibuffer-map nil history)))
    (if (zerop (length string))
        default-value
      string)))

(defun mgl-try (test-name rerun-all)
  "Call the Try test TEST-NAME and display its output in a buffer
with minor mode `mgl-try-mode'.

With a prefix arg, FUNCALL TEST-NAME (see TRY::@IMPLICIT-TRY).
With the default value of TRY:*DEBUG*, this is suitable for
interactive debugging."
  (interactive (list (mgl-try-read-from-minibuffer "Try test"
                                                   (slime-symbol-at-point)
                                                   (car mgl-try-history)
                                                   'mgl-try-history)
                     nil))
  (if (slime-eval '(cl:not (cl:find-package :try)))
      (message "Try is not loaded on the Common Lisp side.")
    (let ((name (if (stringp test-name)
                    `(cl:read-from-string
                      ,test-name)
                  test-name))
          (rerun-msg (if rerun-all
                         " (rerun all)"
                       "")))
      (if current-prefix-arg
          (slime-eval-async `(swank::with-buffer-syntax
                              ()
                              (try::try-for-emacs/implicit ,name))
            (lambda (output)
              (message "Called %S%s" test-name rerun-msg)))
        (slime-eval-async `(swank::with-buffer-syntax
                            ()
                            (try::try-for-emacs ,name :rerun-all ,rerun-all))
          (lambda (output)
            (when (< 0 (length output))
              (mgl-try-display output))
            (message "Tried %S%s" test-name rerun-msg)))))))

(defun mgl-try-display (output)
  (switch-to-buffer "*try*")
  (read-only-mode -1)
  (erase-buffer)
  (lisp-mode)
  (outline-minor-mode)
  (setq outline-regexp "\\*+ ")
  (mgl-try-mode)
  (font-lock-add-keywords
   nil `((,mgl-try-abort-regexp . 'mgl-try-abort-face)
         (,mgl-try-unexpected-failure-regexp . 'mgl-try-unexpected-failure-face)
         (,mgl-try-unexpected-success-regexp . 'mgl-try-unexpected-success-face)
         (,mgl-try-skip-regexp . 'mgl-try-skip-face)
         (,mgl-try-expected-failure-regexp . 'mgl-try-expected-failure-face)
         (,mgl-try-expected-success-regexp . 'mgl-try-expected-success-face)
         (" ⊟[0-9]+" . 'mgl-try-abort-face)
         (" ⊠[0-9]+" . 'mgl-try-unexpected-failure-face)
         (" ⊡[0-9]+" . 'mgl-try-unexpected-success-face)
         (" -[0-9]+" . 'mgl-try-skip-face)
         (" ×[0-9]+" . 'mgl-try-expected-failure-face)
         (" ⋅[0-9]+" . 'mgl-try-expected-success-face)))
  (insert "Legend:\n")
  (mgl-try-insert-with-face "⊟: TRY:ABORT\n" 'mgl-try-abort-face)
  (mgl-try-insert-with-face "⊠: TRY:UNEXPECTED-FAILURE\n"
                            'mgl-try-unexpected-failure-face)
  (mgl-try-insert-with-face "⊡: TRY:UNEXPECTED-SUCCESS\n"
                            'mgl-try-unexpected-success-face)
  (mgl-try-insert-with-face "-: TRY:SKIP\n" 'mgl-try-skip-face)
  (mgl-try-insert-with-face "×: TRY:EXPECTED-FAILURE\n"
                            'mgl-try-expected-failure-face)
  (mgl-try-insert-with-face "⋅: TRY:EXPECTED-SUCCESS\n\n"
                            'mgl-try-expected-success-face)
  (insert output)
  (read-only-mode)
  (outline-show-all)
  (outline-hide-body))

(defun mgl-try-insert-with-face (string face)
  (put-text-property 0 (length string) 'font-lock-face face string)
  (insert string))

(defun mgl-try-rerun-! ()
  "Rerun the most recent trial (TRY:!).
This is subject to TRY:*TRY-RERUN*. See TRY::@RERUN."
  (interactive)
  (mgl-try 'try:! nil))

(defun mgl-try-rerun-!-all ()
  "Rerun the most recently finished test (corresponding to TRY:!).
This unconditionally reruns all tests. It's not subject to
TRY:*TRY-RERUN*."
  (interactive)
  (mgl-try 'try:! t))


(defvar mgl-try-unexpected-regexp
  (concat "\\(" mgl-try-abort-regexp
          "\\)\\|\\(" mgl-try-unexpected-failure-regexp
          "\\)\\|\\(" mgl-try-unexpected-success-regexp "\\)"))

(defun mgl-try-previous-unexpected ()
  "Move point to the previous unexpected event, and show its subtree."
  (interactive)
  (mgl-try-previous-regexp mgl-try-unexpected-regexp "unexpected event"))

(defun mgl-try-next-unexpected ()
  "Move point to the next unexpected event, and show its subtree."
  (interactive)
  (mgl-try-next-regexp mgl-try-unexpected-regexp "unexpected event"))

(defvar mgl-try-not-expected-success-regexp
  (concat "\\(" mgl-try-abort-regexp
          "\\)\\|\\(" mgl-try-unexpected-failure-regexp
          "\\)\\|\\(" mgl-try-unexpected-success-regexp
          "\\)\\|\\(" mgl-try-skip-regexp
          "\\)\\|\\(" mgl-try-expected-failure-regexp "\\)"))

(defun mgl-try-previous-not-expected-success ()
  "Move point to the previous event that's not an expected success,
and show its subtree."
  (interactive)
  (mgl-try-previous-regexp mgl-try-not-expected-success-regexp
                           "event that's not an expected success"))

(defun mgl-try-next-not-expected-success ()
  "Move point to the next event that's not an expected success,
and show its subtree."
  (interactive)
  (mgl-try-next-regexp mgl-try-not-expected-success-regexp
                       "event that's not an expected success"))

(defun mgl-try-previous-regexp (regexp what)
  (if (ignore-errors (search-backward-regexp regexp nil nil))
      (outline-show-subtree)
    (message "No previous %s" what)))

(defun mgl-try-next-regexp (regexp what)
  (let ((pos (save-excursion
               (ignore-errors
                 (search-forward-regexp regexp nil nil)))))
    (if (null pos)
        (message "No next %s" what)
      (ignore-errors (outline-hide-leaves))
      (goto-char pos)
      (outline-show-subtree))))

(provide 'mgl-try)
