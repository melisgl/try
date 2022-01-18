(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @try-manual (:title "Try Manual")
  (try asdf:system)
  (@try/links section)
  (@try/tutorial section)
  (@try/events section)
  (@try/is section)
  (@try/check-library section)
  (@try/tests section)
  (@try/implementation-notes section)
  (@try/glossary section))

(defsection @try/links (:title "Links")
  "Here is the [official repository](https://github.com/melisgl/try)
  and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/try-manual.html)
  for the latest version.")

(defsection @try/tutorial (:title "Tutorial")
  """Try is a library for unit testing with equal support for
  interactive and non-interactive workflows. Tests are functions, and
  almost everything else is a condition, whose types feature
  prominently in parameterization.

  ##### Looking for Truth

  @TRY/IS is a replacement for CL:ASSERT, that can capture values of
  subforms to provide context to failures:

  ```
  (is (= (1+ 5) 0))

  debugger invoked on a TRY:UNEXPECTED-RESULT-FAILURE:
    UNEXPECTED-FAILURE in check:
      (IS (= #1=(1+ 5) 0))
    where
      #1# = 6
  ```

  Note the `#N#` syntax due to *PRINT-CIRCLE*.

  ##### Checking Multiple Values

  IS automatically captures values of arguments to functions like `1+`
  in the above example (see @TRY/AUTOMATIC-CAPTURES). Values of other
  interesting subforms can be explicitly requested to be
  captured (@TRY/EXPLICIT-CAPTURES). IS supports capturing multiple
  values and can be taught how to deal with macros
  (@TRY/WRITING-AUTOMATIC-CAPTURE-RULES). The combination of these
  features allows MATCH-VALUES to be implementable as tiny extension:

  ```
  (is (match-values (values (1+ 5) "sdf")
        (= * 0)
        (string= * "sdf")))

  debugger invoked on a TRY:UNEXPECTED-FAILURE:
    (IS
     (MATCH-VALUES #1=(VALUES (1+ 5) #2="sdf")
       (= * 0)
       (STRING= * "sdf")))
    where
      #1# == 6
             #2#
  ```

  In the body of MATCH-VALUES, [*][dislocated] is bound to successive
  return values of some form, here `(VALUES (1+ 5) "sdf")`.
  MATCH-VALUES comes with an automatic rewrite rule that captures the
  values of this form, which are printed above as `#1# == 6 #2#`. IS
  is flexible enough that all other checks (SIGNALS, SIGNALS-NOT,
  INVOKES-DEBUGGER, INVOKES-DEBUGGER-NOT, FAILS, and IN-TIME are built
  on top of it.

  ##### Writing Tests

  Beyond a fancy ASSERT, Try provides tests, which are Lisp functions
  that record their execution in TRIAL objects. Let's define a test
  and run it:

  ```
  (deftest should-work ()
    (is t))

  (should-work)
  .. SHOULD-WORK            ; TRIAL-START
  ..   ⋅ (IS T)             ; EXPECTED-RESULT-SUCCESS
  .. ⋅ SHOULD-WORK ⋅1       ; EXPECTED-VERDICT-SUCCESS
  ..
  ==> #<TRIAL (SHOULD-WORK) EXPECTED-SUCCESS 0.000s ⋅1>
  ```

  This is an MGL-PAX transcript, output is prefixed with `.. ` and the
  unreadable return value with `==>`. Try is driven by conditions, and
  the comments to the right give the type of the condition that is
  printed on that line. The `⋅` character marks successes.

  We could have run our test with `(TRY 'SHOULD-WORK)`, as well, which
  does pretty much the same thing except it defaults to never entering
  the debugger, whereas calling a test function directly enters the
  debugger on events whose type matches the type in the variable
  *DEBUG*.

  ```
  (try 'should-work)
  .. SHOULD-WORK
  ..   ⋅ (IS T)
  .. ⋅ SHOULD-WORK ⋅1
  ..
  ==> #<TRIAL (SHOULD-WORK) EXPECTED-SUCCESS 0.000s ⋅1>
  ```

  ##### Test Suites

  Test suites are just tests which call other tests.

  ```
  (deftest my-suite ()
    (should-work)
    (is (= (foo) 5)))

  (defun foo ()
    4)

  (try 'my-suite)
  .. MY-SUITE                 ; TRIAL-START
  ..   SHOULD-WORK            ; TRIAL-START
  ..     ⋅ (IS T)             ; EXPECTED-RESULT-SUCCESS
  ..   ⋅ SHOULD-WORK ⋅1       ; EXPECTED-VERDICT-SUCCESS
  ..   ⊠ (IS (= #1=(FOO) 5))  ; UNEXPECTED-RESULT-FAILURE
  ..     where
  ..       #1# = 4
  .. ⊠ MY-SUITE ⊠1 ⋅1         ; UNEXPECTED-VERDICT-FAILURE
  ..
  ==> #<TRIAL (MY-SUITE) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅1>
  ```

  `⊠` marks UNEXPECTED-FAILUREs. Note how the failure of `(IS (= (FOO)
  5))` caused `MY-SUITE` to fail as well.

  ##### Filtering Output

  To focus on the important bits, we can print only the UNEXPECTED
  events:

  ```
  (try 'my-suite :print 'unexpected)
  .. MY-SUITE
  ..   ⊠ (IS (= #1=(FOO) 5))
  ..     where
  ..       #1# = 4
  .. ⊠ MY-SUITE ⊠1 ⋅1
  ..
  ==> #<TRIAL (MY-SUITE) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅1>
  ```

  Note that `SHOULD-WORK` is still run, and its check's success is
  counted as evidenced by`⋅1`. The above effect can also be achieved
  without running the tests again with REPLAY-EVENTS.

  ##### Debugging

  Let's figure out what went wrong:

  ```
  (my-suite)

  ;;; Here the debugger is invoked:
  UNEXPECTED-FAILURE in check:
    (IS (= #1=(FOO) 5))
  where
    #1# = 4
  Restarts:
   0: [RECORD-EVENT] Record the event and continue.
   1: [FORCE-EXPECTED-SUCCESS] Change outcome to TRY:EXPECTED-RESULT-SUCCESS.
   2: [FORCE-UNEXPECTED-SUCCESS] Change outcome to TRY:UNEXPECTED-RESULT-SUCCESS.
   3: [FORCE-EXPECTED-FAILURE] Change outcome to TRY:EXPECTED-RESULT-FAILURE.
   4: [ABORT-CHECK] Change outcome to TRY:RESULT-ABORT*.
   5: [SKIP-CHECK] Change outcome to TRY:RESULT-SKIP.
   6: [RETRY-CHECK] Retry check.
   7: [ABORT-TRIAL] Record the event and abort trial TRY::MY-SUITE.
   8: [SKIP-TRIAL] Record the event and skip trial TRY::MY-SUITE.
   9: [RETRY-TRIAL] Record the event and retry trial TRY::MY-SUITE.
   10: [SET-TRY-DEBUG] Supply a new value for :DEBUG of TRY:TRY.
   11: [RETRY] Retry SLIME interactive evaluation request.
  ```

  In the [SLIME](https://common-lisp.net/project/slime/doc/html/)
  debugger, we press `v` on the frame of the call to `MY-SUITE` to
  navigate to its definition, realize what the problem is and fix
  `FOO`:

  ```
  (defun foo ()
    5)
  ```

  Now, we select the `RETRY-TRIAL` restart, and on the retry
  `MY-SUITE` passes. The full output is:

  ```
  MY-SUITE
    SHOULD-WORK
      ⋅ (IS T)
    ⋅ SHOULD-WORK ⋅1
  WARNING: redefining TRY::FOO in DEFUN
    ⊠ (IS (= #1=(FOO) 5))
      where
        #1# = 4
  MY-SUITE retry #1
    SHOULD-WORK
      ⋅ (IS T)
    ⋅ SHOULD-WORK ⋅1
    ⋅ (IS (= (FOO) 5))
  ⋅ MY-SUITE ⋅2
  ```

  ##### Rerunning Stuff

  Instead of working interactively, one can fix the failing test and
  rerun it. Now, let's fix `MY-SUITE` and rerun it:

  ```
  (deftest my-suite ()
    (should-work)
    (is nil))

  (try 'my-suite)
  .. MY-SUITE
  ..   SHOULD-WORK
  ..     ⋅ (IS T)
  ..   ⋅ SHOULD-WORK ⋅1
  ..   ⊠ (IS NIL)
  .. ⊠ MY-SUITE ⊠1 ⋅1
  ..
  ==> #<TRIAL (MY-SUITE) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅1>

  (deftest my-suite ()
    (should-work)
    (is t))

  (try !)
  .. MY-SUITE
  ..   ⋅ (IS T)
  .. ⋅ MY-SUITE ⋅1
  ..
  ==> #<TRIAL (MY-SUITE) EXPECTED-SUCCESS 0.004s ⋅1>
  ```

  Here, `!` refers to the most recent TRIAL returned by TRY. When a
  trial is passed to TRY or is `FUNCALL`ed, trials in it which match
  the type in TRY's RERUN argument are rerun (here, UNEXPECTED by
  default). `SHOULD-WORK` and its check are `EXPECTED-SUCCESS`es,
  hence they don't match UNEXPECTED and are not rerun (see
  @TRY/RERUN).

  ##### Conditional Execution

  Conditional execution can be achieved simply testing the TRIAL
  object returned by @TRY/TESTS.

  ```
  (deftest my-suite ()
    (when (passedp (should-work))
      (is t :msg "a test that depends on SHOULD-WORK")
      (when (is nil)
        (is nil :msg "never run"))))
  ```

  ##### Skipping

  ```
  (deftest my-suite ()
    (with-skip ((not (server-available-p)))
      (test-server)))

  (deftest test-server ()
    (is t))

  (defun server-available-p ()
    nil)

  (my-suite)
  ==> #<TRIAL (MY-SUITE) EXPECTED-SUCCESS 0.012s>
  ```

  In the above, `TEST-SERVER` was skipped. No checks were made, no
  errors happened, so nothing was printed.

  ##### Expecting Outcomes

  ```
  (deftest known-broken ()
    (with-failure-expected (t)
      (is nil)))

  (known-broken)
  .. KNOWN-BROKEN
  ..   × (IS NIL)
  .. ⋅ KNOWN-BROKEN ×1
  ..
  ==> #<TRIAL (KNOWN-BROKEN) EXPECTED-SUCCESS 0.000s ×1>
  ```

  `×` marks `EXPECTED-SUCCESS`es. `(WITH-SKIP (T) ...)` makes all
  checks successes and failures EXPECTED, which are counted in their
  own *CATEGORIES* by default but don't make the enclosing tests to
  fail. Also see WITH-EXPECTED-OUTCOME.

  ##### Running Tests on Definition

  With *RUN-DEFTEST-WHEN*, one can run test on definition. To run
  tests on evaluation, as in SLIME `C-M-x`, `slime-eval-defun`:

  ```
  (setq *run-deftest-when* :execute)

  (deftest some-test ()
    (is t))
  .. SOME-TEST
  ..   ⋅ (IS T)
  .. ⋅ SOME-TEST ⋅1
  ..
  => SOME-TEST

  (setq *run-deftest-when* nil)
  ```

  ##### Fixtures

  There is no direct support for fixtures in Try. One can easily write
  macros like the following.

  ```
  (defvar *server* nil)

  (defmacro with-xxx (&body body)
    `(flet ((,with-xxx-body ()
              ,@body))
       (if *server*
           (with-xxx-body)
           (with-server (make-expensive-server)
             (with-xxx-body)))))
  ```

  Plus, with support for selectively @TRY/RERUN, the need for fixtures
  is lessened.

  ##### Packages

  The suggested way of writing tests is to call test functions
  explicitly:

  ```
  (defpackage :some-test-package
    (:use #:common-lisp #:try))
  (in-package :some-test-package)

  (deftest test-all ()
    (test-this)
    (test-that))

  (deftest test-this ()
    (test-this/more))

  (deftest test-this/more ()
    (is t))

  (deftest test-that ()
    (is t))

  (deftest not-called ()
    (is t))

  (defun test ()
    (warn-on-tests-not-run ((find-package :some-test-package))
      (try 'test-all)))

  (test)
  .. TEST-ALL
  ..   TEST-THIS
  ..     TEST-THIS/MORE
  ..       ⋅ (IS T)
  ..     ⋅ TEST-THIS/MORE ⋅1
  ..   ⋅ TEST-THIS ⋅1
  ..   TEST-THAT
  ..     ⋅ (IS T)
  ..   ⋅ TEST-THAT ⋅1
  .. ⋅ TEST-ALL ⋅2
  .. WARNING: Test NOT-CALLED not run.
  ==> #<TRIAL (TEST-ALL) EXPECTED-SUCCESS 0.012s ⋅2>
  ```

  Note how the TEST function uses WARN-ON-TESTS-NOT-RUN to catch any
  tests defined in `SOME-TEST-PACKAGE` that were not run. Tests can be
  deleted by FMAKUNBOUND, UNINTERN, or by redefining the function with
  DEFUN. Tests defined in a given package can be listed with
  LIST-PACKAGE-TESTS.

  This style allows higher level tests to establish the dynamic
  environment necessary for lower level tests.
  """)


(defsection @try/implementation-notes (:title "Implementation Notes")
  """Try is supported on ABCL, AllegroCL, CLISP, CCL, CMUCL, ECL and
  SBCL.

  - Pretty printing is non-existent on CLISP and broken on ABCL. The
    output may look garbled on them.

  - Gray streams are broken on ABCL so the output may look even worse
    [https://abcl.org/trac/ticket/373](https://abcl.org/trac/ticket/373).

  - ABCL, CMUCL, and ECL have a bug related to losing
    [EQL][function]ness of source literals
    [https://gitlab.com/embeddable-common-lisp/ecl/-/issues/665](https://gitlab.com/embeddable-common-lisp/ecl/-/issues/665).
    The result is somewhat cosmetic, it may cause multiple captures
    being made for the same thing.
  """)


(defsection @try/glossary (:title "Glossary" :export nil)
  (@function-designator glossary-term)
  (@funcallable-instance glossary-term)
  (@non-local-exit glossary-term)
  (@cancelled-nlx glossary-term))

(define-glossary-term @function-designator (:title "function designator")
  "This is a term from the Common Lisp ANSI standard. A function
  designator is a symbol (denoting the function named by that symbol
  in the global environment), or a function (denoting itself).")

(define-glossary-term @funcallable-instance (:title "funcallable instance")
  "This is a term from the MOP. A funcallable instance is an instance
  of a class that's a subclass of `MOP:FUNCALLABLE-STANDARD-CLASS`. It
  is like a normal instance, but it can also be `FUNCALL`ed.")

(define-glossary-term @non-local-exit (:title "non-local exit")
  "This is a term from the Common Lisp ANSI standard. If a form does
  not return normally, but control is transferred via `GO`, RETURN,
  RETURN-FROM or THROW, then it is said to have performed a non-local
  exit.")

(define-glossary-term @cancelled-nlx (:title "cancelled non-local exit")
  "This is a term from the Common Lisp ANSI standard. If during the
  unwinding of the stack initiated by a @NON-LOCAL-EXIT another nlx is
  initiated in, and exits from an UNWIND-PROTECT cleanup form, then
  this second nlx is said to have cancelled the first, and the first
  nlx will not continue.

  ```
  (catch 'foo
    (catch 'bar
      (unwind-protect
           (throw 'foo 'foo)
        (throw 'bar 'bar))))
  => BAR
  ```")
