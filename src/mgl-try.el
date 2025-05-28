;; -*- lexical-binding: t -*-

(defgroup mgl-try-faces nil
  "Faces in mgl-try buffers."
  :prefix "mgl-try-")

(defface mgl-try-abort-face
  '((t (:inherit error :inverse-video t :box t :bold t)))
  "Face for TRY:ABORT* event markers."
  :group 'mgl-try-faces)

(defface mgl-try-unexpected-failure-face
  '((t (:inherit warning :inverse-video t :box t :bold t)))
  "Face for TRY:UNEXPECTED-FAILURE event markers."
  :group 'mgl-try-faces)

(defface mgl-try-unexpected-success-face
  '((t (:inherit success :inverse-video t :box t :bold t)))
  "Face for TRY:UNEXPECTED-SUCCESS event markers."
  :group 'mgl-try-faces)

(defface mgl-try-skip-face
  '((t (:inherit error)))
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
    (define-key map (kbd "p") 'mgl-try-mode-previous-unexpected)
    (define-key map (kbd "n") 'mgl-try-mode-next-unexpected)
    (define-key map (kbd "P") 'mgl-try-mode-previous-not-expected-success)
    (define-key map (kbd "N") 'mgl-try-mode-next-not-expected-success)
    (define-key map (kbd "t") 'mgl-try)
    (define-key map (kbd "r") 'mgl-try-rerun-!)
    (define-key map (kbd "R") 'mgl-try-rerun-!-all)
    (define-key map (kbd "v") 'mgl-try-mode-show-source)
    map))

(define-minor-mode mgl-try-mode
  "\\<mgl-try-mode-map>\
  mgl-try: a minor mode to interact with the Common Lisp TRY library.
  \\{mgl-try-mode-map}"
  :lighter " Try"
  :keymap mgl-try-mode-map)


;;;; Version

(defun mgl-try-find-file-up (file-name)
  (concat (locate-dominating-file load-file-name file-name)
          file-name))

(defun mgl-try-read-version ()
  (with-temp-buffer
    (insert-file-contents (mgl-try-find-file-up "version.lisp-expr"))
    (goto-char (point-min))
    (read (current-buffer))))

;;; See TRY::CHECK-TRY-ELISP-VERSION.
(defvar mgl-try-version)
;;; The next line is `(setq mgl-try-version (mgl-try-read-version))`
;;; in the sources, which gets replaced by the the version in
;;; `version.lisp-expr` by MGL-TRY:INSTALL-TRY-ELISP.
(setq mgl-try-version (mgl-try-read-version))

(defvar mgl-try-file-name)
(setq mgl-try-file-name load-file-name)

(defun mgl-try-reload ()
  "Reload mgl-try.el. This may be necessary after upgrading Try.
See TRY::@EMACS-SETUP."
  (interactive)
  (let ((sourcefile (concat (file-name-sans-extension mgl-try-file-name)
                            ".el")))
    (load-file sourcefile)))


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

(defun mgl-try (test-name)
  "Call the Try test TEST-NAME and display its output in a buffer
with minor mode `mgl-try-mode'. TEST-NAME defaults to the symbol
under point or, in `mgl-try-mode', to the name of the innermost
global test that contains the current line.

- With no prefix arg, TEST-NAME is invoked by the function
  TRY:TRY (an TRY::@EXPLICIT-TRY). With the default value of
  TRY:*TRY-DEBUG* being NIL, this will not enter the debugger.

- With a prefix arg, TEST-NAME is FUNCALLed (an
  TRY::@IMPLICIT-TRY). With the default value of TRY:*DEBUG*,
  this is suitable for interactive debugging.

As long as the `mgl-try-mode' buffer exists, `mgl-try' (bound
\"t\" in that buffer), will use the trial with which the buffer
was created as TRY:*RECORD-CONTEXT* (without affecting it global
binding). Thus, all invocations of `mgl-try' will go to the same
buffer and use the dynamic environment of the first trial. If
that's no longer desired, just kill the buffer (bound to \"q\").

To ensure that the output is parsable by Elisp, the following
Common Lisp variables are overridden:

- All trials are collected. That is, TRY:*COLLECT* and
  TRY:*TRY-COLLECT* (in implicit and explicit modes) are set to
  TRIAL-EVENT.

- TRY:*TRY-PRINTER* and TRY:*PRINTER* are set to TRY:TREE-PRINTER,

- TRY:*PRINT-PARENT* to T,

- TRY:*PRINT-INDENTATION* to :OUTLINE,

- TRY:*PRINT-COMPACTLY* to NIL,

- TRY:*DEFER-DESCRIBE* to NIL.

- TRY:*CATEGORIES* to TRY:FANCY-STD-FAILURE (with a small
  modification to print UNEXPECTED-VERDICT-FAILUREs with the
  marker \"→⊠\". This is because their failure is a consequence
  of other failures, and this way `mgl-try-mode-next-unexpected'
  skips over them.

Other variables not listed here (such as TRY:*PRINT-BACKTRACE*,
TRY:*DEBUG*, TRY:*TRY-DEBUG*) are in effect."
  (interactive (list (mgl-try-read-from-minibuffer
                      "Try test"
                      (mgl-try-default-test-name)
                      (car mgl-try-history)
                      'mgl-try-history)))
  (mgl-try* test-name nil))

(defun mgl-try-default-test-name ()
  (if (not mgl-try-mode)
      (slime-symbol-at-point)
    (save-excursion
      (beginning-of-line)
      (cl-loop
       (let ((test-name (mgl-try-mode-deftest-name-on-current-line)))
         (when test-name
           (cl-return test-name)))
       (let ((orig-point (point)))
         ;; `outline-up-heading' is tempting, but it finds the parent
         ;; for verdict lines.
         (outline-previous-visible-heading 1)
         (when (= (point) orig-point)
           (cl-return)))))))

(defun mgl-try-mode-deftest-name-on-current-line ()
  (when (looking-at "\\*")
    (slime-symbol-at-point)
    (search-forward " " (line-end-position))
    ;; TRY:TRIAL-START events have either a normal entry or a skip
    ;; marker.
    (when (search-forward-regexp "[→-] " (+ (point) 2) t)
      (slime-symbol-at-point))))

(defvar mgl-try-buffer-name "*try*")

(defun mgl-try* (test-name rerun)
  (if (slime-eval '(cl:null (cl:find-package :try)))
      (message "Try is not loaded on the Common Lisp side.")
    (slime-eval `(try::check-try-elisp-version ',mgl-try-version))
    (let ((name (if (stringp test-name)
                    `(cl:read-from-string
                      ,test-name)
                  test-name))
          (implicit (not (null current-prefix-arg))))
      (slime-eval-async `(swank::with-buffer-syntax
                          ()
                          (try::try-for-emacs
                           ,name :rerun ,rerun
                           :implicit ,implicit
                           :set-rerun-context ,(null (get-buffer
                                                      mgl-try-buffer-name))))
        (lambda (output)
          (when (or (< 0 (length output))
                    mgl-try-mode)
            (mgl-try-display output))
          (let ((action (if implicit "Called" "Tried"))
                (rerun-msg (if (eq rerun t)
                               " (rerun all)"
                             "")))
            (message "%s %S%s" action test-name rerun-msg)))))))

(defun mgl-try-display (output)
  (let ((package (slime-current-package)))
    (pop-to-buffer mgl-try-buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (lisp-mode)
    (setq slime-buffer-package package)
    (outline-minor-mode)
    (setq outline-regexp "\\*+ ")
    (mgl-try-mode)
    (font-lock-add-keywords
     nil `((,mgl-try-abort-regexp . 'mgl-try-abort-face)
           (,mgl-try-unexpected-failure-regexp
            . 'mgl-try-unexpected-failure-face)
           (,mgl-try-unexpected-success-regexp
            . 'mgl-try-unexpected-success-face)
           (,mgl-try-skip-regexp . 'mgl-try-skip-face)
           (,mgl-try-expected-failure-regexp . 'mgl-try-expected-failure-face)
           (,mgl-try-expected-success-regexp . 'mgl-try-expected-success-face)
           ("⊟[0-9]+" . 'mgl-try-abort-face)
           ("⊠[0-9]+" . 'mgl-try-unexpected-failure-face)
           ("⊡[0-9]+" . 'mgl-try-unexpected-success-face)
           ("-[0-9]+" . 'mgl-try-skip-face)
           ("×[0-9]+" . 'mgl-try-expected-failure-face)
           ("⋅[0-9]+" . 'mgl-try-expected-success-face)))
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
    (outline-hide-body)
    (mgl-try-flash-region (point-min) (point-max))
    (goto-char (point-min))
    (mgl-try-mode-next-not-skip)
    (ignore-errors
      (outline-hide-subtree)
      (outline-previous-visible-heading 1))))

(defun mgl-try-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun mgl-try-insert-with-face (string face)
  (put-text-property 0 (length string) 'font-lock-face face string)
  (insert string))

(defun mgl-try-rerun-! ()
  "Rerun the most recent trial conducted by Emacs (this is
distinct from TRY:!). See TRY:*TRY-RERUN* and TRY::@RERUN.

Prefix arguments are variables overrides are as described in
`mgl-try'."
  (interactive)
  (mgl-try* 'try::*emacs-!* :unspecified))

(defun mgl-try-rerun-!-all ()
  "Like `mgl-try-rerun-!', but TRY:*TRY-RERUN* and
TRY:*RERUN* are ignored, and all test are rerun.

Prefix arguments are variables overrides are as described in
`mgl-try'."
  (interactive)
  (mgl-try* 'try::*emacs-!* t))


(defvar mgl-try-unexpected-regexp
  (concat "\\(" mgl-try-abort-regexp
          "\\)\\|\\(" mgl-try-unexpected-failure-regexp
          "\\)\\|\\(" mgl-try-unexpected-success-regexp "\\)"))

(defun mgl-try-mode-previous-unexpected ()
  "Move point to the previous unexpected event, and show its subtree."
  (interactive)
  (mgl-try-mode-previous-regexp mgl-try-unexpected-regexp "unexpected event"))

(defun mgl-try-mode-next-unexpected ()
  "Move point to the next unexpected event, and show its subtree."
  (interactive)
  (mgl-try-mode-next-regexp mgl-try-unexpected-regexp "unexpected event"))

(defvar mgl-try-not-expected-success-regexp
  (concat "\\(" mgl-try-abort-regexp
          "\\)\\|\\(" mgl-try-unexpected-failure-regexp
          "\\)\\|\\(" mgl-try-unexpected-success-regexp
          "\\)\\|\\(" mgl-try-skip-regexp
          "\\)\\|\\(" mgl-try-expected-failure-regexp "\\)"))

(defun mgl-try-mode-previous-not-expected-success ()
  "Move point to the previous event that's not an expected success,
and show its subtree."
  (interactive)
  (mgl-try-mode-previous-regexp mgl-try-not-expected-success-regexp
                           "event that's not an expected success"))

(defun mgl-try-mode-next-not-expected-success ()
  "Move point to the next event that's not an expected success,
and show its subtree."
  (interactive)
  (mgl-try-mode-next-regexp mgl-try-not-expected-success-regexp
                       "event that's not an expected success"))

(defvar mgl-try-not-skip-regexp
  (concat "\\(" mgl-try-abort-regexp
          "\\)\\|\\(" mgl-try-unexpected-failure-regexp
          "\\)\\|\\(" mgl-try-unexpected-success-regexp
          "\\)\\|\\(" mgl-try-expected-success-regexp
          "\\)\\|\\(" mgl-try-expected-failure-regexp "\\)"))

(defun mgl-try-mode-previous-not-skip-success ()
  "Move point to the previous event that's not a TRY:SKIP,
and show its subtree."
  (interactive)
  (mgl-try-mode-previous-regexp mgl-try-not-skip-regexp
                           "event that's not a skip"))

(defun mgl-try-mode-next-not-skip ()
  "Move point to the next event that's not a TRY:SKIP,
and show its subtree."
  (interactive)
  (mgl-try-mode-next-regexp mgl-try-not-skip-regexp
                       "event that's not a skip"))

(defun mgl-try-mode-previous-regexp (regexp what)
  (if (null (ignore-errors (search-backward-regexp regexp nil nil)))
      (message "No previous %s" what)
    (beginning-of-line)
    (outline-show-subtree)))

(defun mgl-try-mode-next-regexp (regexp what)
  (let ((pos (save-excursion
               (ignore-errors
                 (search-forward-regexp regexp nil nil)))))
    (if (null pos)
        (when (called-interactively-p 'any)
          (message "No next %s" what))
      (ignore-errors (outline-hide-leaves))
      (goto-char pos)
      (beginning-of-line)
      (outline-show-subtree))))

(defun mgl-try-mode-show-source ()
  (interactive)
  (when mgl-try-mode
    (let ((test-name (mgl-try-default-test-name)))
      (when test-name
        (slime-edit-definition test-name)))))

(provide 'mgl-try)
