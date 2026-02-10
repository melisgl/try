(in-package :try)

;;; Called when generating documentation and a code block like this is
;;; encountered:
;;;
;;; ```cl-transcript (:dynenv try-transcript)
;;; ```
(defun try-transcript (fn)
  (without-redefinition-warnings
    (let ((*package* (find-package :try))
          (*run-deftest-when* *run-deftest-when*)
          (*testing-timing* '(0.1 0.2 0.3 0.4 0.5 0.6))
          (*categories* (fancy-std-categories))
          (*rerun-context* nil)
          (mgl-pax:*transcribe-check-consistency*
            '((:output try-transcript-output=)
              (:readable equal)
              (:unreadable try-transcript-unreadable=))))
      (funcall fn))))

(defun try-transcript-output= (string1 string2)
  (string= (try-transcript-normalize-output string1)
           (try-transcript-normalize-output string2)))

(defun try-transcript-normalize-output (string)
  (try-transcript-upcase-trial
   (squeeze-whitespace (delete-trailing-whitespace (delete-comments string)))))

(defun try-transcript-unreadable= (string1 string2)
  (string= (try-transcript-normalize-unreadable string1)
           (try-transcript-normalize-unreadable string2)))

(defun try-transcript-normalize-unreadable (string)
  (try-transcript-upcase-trial
   ;; Replace the time in "#<TRIAL UNEXPECTED 0.001s>" with "0.000s".
   (cl-ppcre:regex-replace-all " \\d.\\d\\d\\ds" string " 0.000s")))

(defun try-transcript-upcase-trial (string)
  ;; ECL's PRINT-UNREADABLE-OBJECT prints in lowercase #<trial ..>.
  #+ecl (cl-ppcre:regex-replace-all "#<trial" string "#<TRIAL")
  #-ecl string)


;;;; Register in PAX World

(defun try-sections ()
  (list @try-manual))

(defun try-pages ()
  `((:objects
     (, @try-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :try
                      "https://github.com/melisgl/try"))))

(register-doc-in-pax-world :try (try-sections) (try-pages))


(defun try-pages* (format &key (output-dir ""))
  (let ((source-uri-fn (make-git-source-uri-fn
                        "try"
                        "https://github.com/melisgl/try"))
        (try-file (ecase format
                    ((:plain) "README")
                    ((:markdown) "README.md")
                    ((:html) "doc/try-manual.html")
                    ((:pdf) "try-manual.pdf")))
        (output-dir (asdf:system-relative-pathname "try" output-dir)))
    `((:objects (, @try-manual)
       :output (,(merge-pathnames try-file output-dir)
                ,@pax::*default-output-options*)
       ,@(when (member format '(:plain :markdown))
           '(:footer-fn pax::print-markdown-footer))
       :uri-fragment ,try-file
       :source-uri-fn ,source-uri-fn))))

(defun update-try-docs (&key (output-dir ""))
  (let ((*document-url-versions* '(1))
        (*document-max-numbering-level* 4)
        (*document-max-table-of-contents-level* 4)
        (*document-html-max-navigation-table-of-contents-level* 3))
    (document (try-sections)
              :pages (try-pages* :plain :output-dir output-dir)
              :format :plain)
    (document (try-sections)
              :pages (try-pages* :markdown :output-dir output-dir)
              :format :markdown)
    #+nil
    (update-asdf-system-html-docs @try-manual :try
                                  :pages (try-pages* :html))))

;;; Regenerate documentation
#+nil
(update-try-docs)
