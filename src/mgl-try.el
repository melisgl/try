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

(defvar mgl-try-unexpected-regexp
  (concat "\\(" mgl-try-abort-regexp
          "\\)\\|\\(" mgl-try-unexpected-failure-regexp
          "\\)\\|\\(" mgl-try-unexpected-success-regexp "\\)"))

(defvar mgl-try-history '()
  "History list of expressions read from the minibuffer.")

(defvar mgl-try-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") 'outline-cycle)
    (define-key map "P" 'outline-previous-visible-heading)
    (define-key map "N" 'outline-next-visible-heading)
    (define-key map "U" 'outline-up-heading)
    (define-key map "p" 'mgl-try-previous-unexpected)
    (define-key map "n" 'mgl-try-next-unexpected)
    (define-key map "t" 'mgl-try-try)
    (define-key map "r" 'mgl-try-rerun-!)
    (define-key map "R" 'mgl-try-rerun-!-all)
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
with minor mode `mgl-try-mode'."
  (interactive (list (mgl-try-read-from-minibuffer "Run test" nil
                                                   (car mgl-try-history)
                                                   'mgl-try-history)
                     nil))
  (slime-eval-async `(swank::with-buffer-syntax
                      ()
                      (try::try-for-emacs ,(if (stringp test-name)
                                               `(cl:read-from-string
                                                 ,test-name)
                                             test-name)
                                          :rerun-all ,rerun-all))
    'mgl-try-display))

(defun mgl-try-display (output)
  (switch-to-buffer "*try*")
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
  (insert output)
  (outline-show-all)
  (outline-hide-body))

(defun mgl-try-try (test-name)
  "Like `mgl-try' but defaults to the symbol under point."
  (interactive (list (mgl-try-read-from-minibuffer "Run test"
                                                   (slime-symbol-at-point)
                                                   (car mgl-try-history)
                                                   'mgl-try-history)))
  (mgl-try test-name nil))

(defun mgl-try-rerun-! ()
  "Rerun the most recent trial (TRY:!).
This is subject to TRY:*TRY-RERUN*. See TRY::@TRY/RERUN."
  (interactive)
  (mgl-try 'try:! nil))

(defun mgl-try-rerun-!-all ()
  "Rerun the most recently finished test (corresponding to TRY:!).
This unconditionally reruns all tests. It's not subject to
TRY:*TRY-RERUN*."
  (interactive)
  (mgl-try 'try:! t))

(defun mgl-try-previous-unexpected ()
  "Move point to the previous unexpected event and show its subtree."
  (interactive)
  (when (ignore-errors
          (search-backward-regexp mgl-try-unexpected-regexp nil nil))
    (outline-show-subtree)))

(defun mgl-try-next-unexpected ()
  "Move point to the next unexpected event and show its subtree."
  (interactive)
  (let ((pos (save-excursion
               (ignore-errors
                 (search-forward-regexp mgl-try-unexpected-regexp nil nil)))))
    (when pos
      (ignore-errors (outline-hide-leaves))
      (goto-char pos)
      (outline-show-subtree))))

(provide 'mgl-try)
