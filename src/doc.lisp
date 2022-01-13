(in-package :try)

;;;; Register in PAX World

(defun pax-sections ()
  (list @try-manual))
(defun pax-pages ()
  `((:objects
     (, @try-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :try
                      "https://github.com/melisgl/try"))))
(register-doc-in-pax-world :try (pax-sections) (pax-pages))

;;; Regenerate documentation
#+nil
(let ((pax:*document-max-numbering-level* 4)
      (pax:*document-max-table-of-contents-level* 4)
      (pax:*document-html-max-navigation-table-of-contents-level* 3))
  (update-asdf-system-readmes @try-manual :try)
  (update-asdf-system-html-docs @try-manual :try
                                :pages (pax-pages)))
