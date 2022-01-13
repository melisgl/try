<a id='x-28TRY-3A-40TRY-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Try Manual

## Table of Contents

- [1 TRY ASDF System Details][65b2]
- [2 Links][a53e]
- [3 Tutorial][7f53]
- [4 Events][68a8]
    - [4.1 Middle Layer of Events][a57a]
    - [4.2 Concrete Events][8a03]
    - [4.3 Event Glue][57b2]
    - [4.4 Printing Events][953d]
    - [4.5 Event Restarts][e852]
    - [4.6 Outcomes][95c4]
        - [4.6.1 Outcome Restarts][14a6]
        - [4.6.2 Checks][3e2d]
            - [4.6.2.1 Check Restarts][dbd2]
        - [4.6.3 Trials][abcf]
            - [4.6.3.1 Trial Events][19eb]
            - [4.6.3.2 Trial Verdicts][ea26]
            - [4.6.3.3 Trial Restarts][dbf0]
    - [4.7 Errors][bbc4]
    - [4.8 Categories][f1c6]
- [5 The IS Macro][808e]
    - [5.1 Format Specifier Forms][22e6]
    - [5.2 Captures][a03c]
        - [5.2.1 Automatic Captures][532a]
            - [5.2.1.1 Writing Automatic Capture Rules][afcd]
        - [5.2.2 Explicit Captures][8a79]
- [6 Check Library][4fbb]
    - [6.1 Checking Conditions][1955]
    - [6.2 Miscellaneous Checks][3ea6]
    - [6.3 Check Utilities][6b07]
        - [6.3.1 Comparing Floats][b594]
- [7 Tests][51bb]
    - [7.1 Calling Test Functions][0fb2]
    - [7.2 Explicit TRY][6c25]
        - [7.2.1 Testables][d270]
        - [7.2.2 Implementation of Implicit TRY][303f]
    - [7.3 Printing Events][53fc]
    - [7.4 Counting Events][3c27]
    - [7.5 Collecting Events][0a38]
    - [7.6 Rerunning Trials][76af]
    - [7.7 Reprocessing Trials][b0a8]
- [8 Implementation Notes][300f]
- [9 Glossary][8292]

###### \[in package TRY\]
<a id='x-28-23A-28-283-29-20BASE-CHAR-20-2E-20-22try-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 TRY ASDF System Details

- Version: 0.0.1
- Description: Try is a test framework.
- Long Description: Try is what we get if we make tests functions and
  build a test framework on top of the condition system like
  [Stefil](https://common-lisp.net/project/stefil/index-old.shtml)
  did, but also address the issue of rerunning and replaying, make the
  [`IS`][ff02] check more capable, use the types of the condition hierarchy to
  parameterize what to debug, print, rerun, and document the whole
  thing.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://melisgl.github.io/try](http://melisgl.github.io/try)
- Bug tracker: [https://github.com/melisgl/try/issues](https://github.com/melisgl/try/issues)
- Source control: [GIT](https://github.com/melisgl/try.git)

<a id='x-28TRY-3A-40TRY-2FLINKS-20MGL-PAX-3ASECTION-29'></a>

## 2 Links

Here is the [official repository](https://github.com/melisgl/try)
and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/try-manual.html)
for the latest version.

<a id='x-28TRY-3A-40TRY-2FTUTORIAL-20MGL-PAX-3ASECTION-29'></a>

## 3 Tutorial

Try is a library for unit testing with equal support for
interactive and non-interactive workflows. Tests are functions, and
almost everything else is a condition, whose types feature
prominently in parameterization.

##### Looking for Truth

[The IS Macro][808e] is a replacement for `CL:ASSERT`, that can capture values of
subforms to provide context to failures:

```
(is (= (1+ 5) 0))

debugger invoked on a TRY:UNEXPECTED-RESULT-FAILURE:
  UNEXPECTED-FAILURE in check:
    (IS (= #1=(1+ 5) 0))
  where
    #1# = 6
```

Note the `#N#` syntax due to `*PRINT-CIRCLE*`.

##### Checking Multiple Values

[`IS`][ff02] automatically captures values of arguments to functions like `1+`
in the above example (see [Automatic Captures][532a]). Values of other
interesting subforms can be explicitly requested to be
captured ([Explicit Captures][8a79]). `IS` supports capturing multiple
values and can be taught how to deal with macros
([Writing Automatic Capture Rules][afcd]). The combination of these
features allows [`MATCH-VALUES`][1f28] to be implementable as tiny extension:

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

In the body of `MATCH-VALUES`, `*` is bound to successive return
values of some form, here `(VALUES (1+ 5) "sdf")`. `MATCH-VALUES`
comes with an automatic rewrite rule that captures the values of
this form, which are printed above as `#1# == 6 #2#`. `IS` is flexible
enough that all other checks ([`SIGNALS`][6ebb], [`SIGNALS-NOT`][1a75],
[`INVOKES-DEBUGGER`][9539], [`INVOKES-DEBUGGER-NOT`][b12f], [`FAILS`][20b7], and [`IN-TIME`][6249] are built
on top of it.

##### Writing Tests

Beyond a fancy `ASSERT`, Try provides tests, which are Lisp functions
that record their execution in [`TRIAL`][9fc3] objects. Let's define a test
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

This is an `MGL-PAX` transcript, output is prefixed with `..` and the
unreadable return value with `==>`. Try is driven by conditions, and
the comments to the right give the type of the condition that is
printed on that line. The `⋅` character marks successes.

We could have run our test with `(TRY 'SHOULD-WORK)`, as well, which
does pretty much the same thing except it defaults to never entering
the debugger, whereas calling a test function directly enters the
debugger on events whose type matches the type in the variable
[`*DEBUG*`][0482].

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

`⊠` marks [`UNEXPECTED-FAILURE`][10b2]s. Note how the failure of `(IS (= (FOO)
5))` caused `MY-SUITE` to fail as well.

##### Filtering Output

To focus on the important bits, we can print only the [`UNEXPECTED`][077b]
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
without running the tests again with [`REPLAY-EVENTS`][73e6].

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

Now, we select the [`RETRY-TRIAL`][93e2] restart, and on the retry
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

Here, [`!`][d0fc] refers to the most recent `TRIAL` returned by [`TRY`][7a62]. When a
trial is passed to `TRY` or is `FUNCALL`ed, trials in it which match
the type in `TRY`'s `RERUN` argument are rerun (here, `UNEXPECTED` by
default). `SHOULD-WORK` and its check are [`EXPECTED-SUCCESS`][7de6]es,
hence they don't match `UNEXPECTED` and are not rerun (see
[Rerunning Trials][76af]).

##### Conditional Execution

Conditional execution can be achieved simply testing the `TRIAL`
object returned by [Tests][51bb].

```
(deftest my-suite ()
  (when (passedp (should-work))
    (is t :msg "a test that depends on SHOULD-WORK")))
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
checks successes and failures [`EXPECTED`][7694], which are counted in their
own [`*CATEGORIES*`][3d4c] by default but don't make the enclosing tests to
fail. Also see [`WITH-EXPECTED-OUTCOME`][ab7a].

##### Running Tests on Definition

With [`*RUN-DEFTEST-WHEN*`][8def], one can run test on definition. To run
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

Plus, with support for selectively [Rerunning Trials][76af], the need for fixtures
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

Note how the `TEST` function uses [`WARN-ON-TESTS-NOT-RUN`][a19c] to catch any
tests defined in `SOME-TEST-PACKAGE` that were not run. Tests can be
deleted by `FMAKUNBOUND`, `UNINTERN`, or by redefining the function with
`DEFUN`. Tests defined in a given package can be listed with
[`LIST-PACKAGE-TESTS`][9e9e].

This style allows higher level tests to establish the dynamic
environment necessary for lower level tests.

<a id='x-28TRY-3A-40TRY-2FEVENTS-20MGL-PAX-3ASECTION-29'></a>

## 4 Events

Try is built around events implemented as `CONDITION`s.
Matching the types of events to [`*DEBUG*`][0482], [`*COUNT*`][7eb2], [`*COLLECT*`][1dfa], [`*RERUN*`][0e2a],
[`*PRINT*`][406c], and [`*DESCRIBE*`][95df] is what gives Try its flexibility.

<a id='x-28TRY-3A-40TRY-2FMIDDLE-LAYER-OF-EVENTS-20MGL-PAX-3ASECTION-29'></a>

### 4.1 Middle Layer of Events

The event hierarchy is fairly involved, so let's start in the middle.
The condition [`EVENT`][6ded] has 4 disjoint subclasses:

- [`TRIAL-START`][a231], which corresponds to the entry to a test (see
  [Tests][51bb]),

- [`VERDICT`][5976], the [`OUTCOME`][a306] of a [`TRIAL`][9fc3],

- [`RESULT`][3091], the `OUTCOME` of a check (see [Checks][3e2d]), and

- [`ERROR*`][e6dd], an unexpected `CL:ERROR` or unadorned [non-local exit][c4d2].

```
(let (;; We don't want to debug nor print a backtrace for the error below.
      (*debug* nil)
      (*describe* nil))
  ;; signals TRIAL-START / VERDICT-ABORT* on entry / exit
  (with-test (demo)
    ;; signals EXPECTED-RESULT-SUCCESS
    (is t)
    ;; signals UNHANDLED-ERROR with a nested CL:ERROR
    (error "xxx")))
.. DEMO                       ; TRIAL-START
..   ⋅ (IS T)                 ; EXPECTED-RESULT-SUCCESS (⋅)
..   ⊟ "xxx" (SIMPLE-ERROR)   ; UNHANDLED-ERROR (⊟)
.. ⊟ DEMO ⊟1 ⋅1               ; VERDICT-ABORT* (⊟)
..
==> #<TRIAL (WITH-TEST (DEMO)) ABORT* 0.004s ⊟1 ⋅1>
```


<a id='x-28TRY-3A-40TRY-2FCONCRETE-EVENTS-20MGL-PAX-3ASECTION-29'></a>

### 4.2 Concrete Events

The non-abstract condition classes of events that are actually
signalled are called concrete.

[`TRIAL-START`][a231] is a concrete event class. [`RESULT`][3091]s and [`VERDICT`][5976]s have six
 concrete subclasses:

- [`EXPECTED-RESULT-SUCCESS`][1c37], [`UNEXPECTED-RESULT-SUCCESS`][87a0],
   [`EXPECTED-RESULT-FAILURE`][16a7], [`UNEXPECTED-RESULT-FAILURE`][8ce0],
   [`RESULT-SKIP`][989e], [`RESULT-ABORT*`][4c47]

- [`EXPECTED-VERDICT-SUCCESS`][1431], [`UNEXPECTED-VERDICT-SUCCESS`][bd26],
   [`EXPECTED-VERDICT-FAILURE`][fec9], [`UNEXPECTED-VERDICT-FAILURE`][c04d],
   [`VERDICT-SKIP`][24b5], [`VERDICT-ABORT*`][ca88]

[`ERROR*`][e6dd] is an abstract class with two concrete subclasses:

- [`UNHANDLED-ERROR`][e65b], signalled when a `CL:ERROR` reaches the handler set
   up by [`DEFTEST`][e6a7] or [`WITH-TEST`][af8d], or when the debugger is invoked.

- [`NLX`][d43d], signalled when no error was detected by the handler, but the
   trial finishes with a [non-local exit][c4d2].

These are the 15 concrete event classes.

<a id='x-28TRY-3A-40TRY-2FEVENT-GLUE-20MGL-PAX-3ASECTION-29'></a>

### 4.3 Event Glue

These condition classes group various bits of the
[Concrete Events][8a03] and the [Middle Layer of Events][a57a] for ease of
reference.

Concrete event classes except [`TRIAL-START`][a231] are subclasses of
hyphen-separated words in their name. For example,
[`UNEXPECTED-RESULT-FAILURE`][8ce0] inherits from [`UNEXPECTED`][077b], [`RESULT`][3091], and
[`FAILURE`][ea4a], so it matches types such as `UNEXPECTED` or `(AND UNEXPECTED
RESULT)`.

<a id='x-28TRY-3AEVENT-20CONDITION-29'></a>

- [condition] **EVENT**

    Common abstract superclass of all events in Try.

<a id='x-28TRY-3AEXPECTED-20CONDITION-29'></a>

- [condition] **EXPECTED** *EVENT*

    Concrete condition classes with `EXPECTED` in their
    name are subclasses of `EXPECTED`. [`SKIP`][5918] is also a subclass of
    `EXPECTED`.

<a id='x-28TRY-3AUNEXPECTED-20CONDITION-29'></a>

- [condition] **UNEXPECTED** *EVENT*

    Concrete condition classes with `UNEXPECTED` in their
    name are subclasses of `UNEXPECTED`. [`ABORT*`][cca5] is also a subclass of
    `UNEXPECTED`.

<a id='x-28TRY-3ASUCCESS-20CONDITION-29'></a>

- [condition] **SUCCESS** *EVENT*

    See [Checks][3e2d] and [Trial Verdicts][ea26] for how
    `SUCCESS` or [`FAILURE`][ea4a] is decided.

<a id='x-28TRY-3AFAILURE-20CONDITION-29'></a>

- [condition] **FAILURE** *EVENT*

    See [`SUCCESS`][440d].

<a id='x-28TRY-3ADISMISSAL-20CONDITION-29'></a>

- [condition] **DISMISSAL** *EVENT*

    The third possibility after [`SUCCESS`][440d] and [`FAILURE`][ea4a].
    Either [`SKIP`][5918] or [`ABORT*`][cca5].

<a id='x-28TRY-3AABORT-2A-20CONDITION-29'></a>

- [condition] **ABORT\*** *UNEXPECTED*

    [`RESULT-ABORT*`][4c47], [`VERDICT-ABORT*`][ca88] or [`ERROR*`][e6dd].

<a id='x-28TRY-3ASKIP-20CONDITION-29'></a>

- [condition] **SKIP** *EXPECTED DISMISSAL*

    [`RESULT-SKIP`][989e] or [`VERDICT-SKIP`][24b5].

<a id='x-28TRY-3ALEAF-20CONDITION-29'></a>

- [condition] **LEAF** *EVENT*

    [`RESULT`][3091] or [`ERROR*`][e6dd].

<a id='x-28TRY-3AEXPECTED-SUCCESS-20TYPE-29'></a>

- [type] **EXPECTED-SUCCESS**

    A shorthand for `(AND EXPECTED SUCCESS)`.

<a id='x-28TRY-3AUNEXPECTED-SUCCESS-20TYPE-29'></a>

- [type] **UNEXPECTED-SUCCESS**

    A shorthand for `(AND UNEXPECTED SUCCESS)`.

<a id='x-28TRY-3AEXPECTED-FAILURE-20TYPE-29'></a>

- [type] **EXPECTED-FAILURE**

    A shorthand for `(AND EXPECTED FAILURE)`.

<a id='x-28TRY-3AUNEXPECTED-FAILURE-20TYPE-29'></a>

- [type] **UNEXPECTED-FAILURE**

    A shorthand for `(AND UNEXPECTED FAILURE)`.

<a id='x-28TRY-3APASS-20TYPE-29'></a>

- [type] **PASS**

    An [`OUTCOME`][a306] that's not an [`ABORT*`][cca5] or an [`UNEXPECTED`][077b] [`FAILURE`][ea4a].

<a id='x-28TRY-3AFAIL-20TYPE-29'></a>

- [type] **FAIL**

    An [`ABORT*`][cca5] or an [`UNEXPECTED`][077b] [`FAILURE`][ea4a].

<a id='x-28TRY-3A-40TRY-2FPRINTING-EVENTS-20MGL-PAX-3ASECTION-29'></a>

### 4.4 Printing Events

<a id='x-28TRY-3A-2AEVENT-PRINT-BINDINGS-2A-20VARIABLE-29'></a>

- [variable] **\*EVENT-PRINT-BINDINGS\*** *((\*PRINT-CIRCLE\* T))*

    [`EVENT`][6ded]s are conditions signalled in code that may change printer
    variables such as `*PRINT-CIRCLE*`, `*PRINT-LENGTH*`, etc. To control
    how events are printed, the list of variable bindings in
    `*EVENT-PRINT-BINDINGS*` is established whenever an `EVENT` is printed
    as if with:
    
    ```
    (progv (mapcar #'first *event-print-bindings*)
           (mapcar #'second *event-print-bindings*)
      ...)
    ```
    
    The default value ensures that shared structure is recognized (see
    [Captures][a03c]). If the `#N#` syntax feels cumbersome, then change
    this variable.

<a id='x-28TRY-3A-40TRY-2FEVENT-RESTARTS-20MGL-PAX-3ASECTION-29'></a>

### 4.5 Event Restarts

Only [`RECORD-EVENT`][318d] is applicable to all [`EVENT`][6ded]s. See
[Check Restarts][dbd2], [Trial Restarts][dbf0] for more.

<a id='x-28TRY-3ARECORD-EVENT-20FUNCTION-29'></a>

- [function] **RECORD-EVENT** *&OPTIONAL CONDITION*

    This restart is always the first restart available when an [`EVENT`][6ded] is
    signalled running under [`TRY`][7a62] (i.e. there is a [`CURRENT-TRIAL`][e542]). `TRY`
    always invokes `RECORD-EVENT` when handling events.

<a id='x-28TRY-3A-40TRY-2FOUTCOMES-20MGL-PAX-3ASECTION-29'></a>

### 4.6 Outcomes

<a id='x-28TRY-3AOUTCOME-20CONDITION-29'></a>

- [condition] **OUTCOME** *EVENT*

    An `OUTCOME` is the resolution of either a [`TRIAL`][9fc3] or a
    check (see [Checks][3e2d]), corresponding to subclasses [`VERDICT`][5976] and
    [`RESULT`][3091].

<a id='x-28TRY-3AWITH-EXPECTED-OUTCOME-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-EXPECTED-OUTCOME** *(EXPECTED-TYPE) &BODY BODY*

    When an [`OUTCOME`][a306] is to be signalled, `EXPECTED-TYPE` determines
    whether it's going to be [`EXPECTED`][7694]. The concrete `OUTCOME` classes are
    `{EXPECTED,UNEXPECTED}-{RESULT,VERDICT}-{SUCCESS,FAILURE}` (see
    [Events][68a8]), of which [`RESULT`][3091] or [`VERDICT`][5976] and [`SUCCESS`][440d] or [`FAILURE`][ea4a] are
    already known. If a `RESULT` `FAILURE` is to be signalled, then the
    moral equivalent of `(SUBTYPEP '(AND RESULT FAILURE) EXPECTED-TYPE)`
    is evaluated and depending on whether it's true,
    [`EXPECTED-RESULT-FAILURE`][16a7] or [`UNEXPECTED-RESULT-FAILURE`][8ce0] is signalled.
    
    By default, `SUCCESS` is expected. The following example shows how to
    expect both `SUCCESS` and `FAILURE` for `RESULT`s, while requiring
    `VERDICT`s to succeed:
    
    ```
    (let ((*debug* nil))
      (with-expected-outcome ('(or result (and verdict success)))
        (with-test (t1)
          (is nil))))
    .. T1
    ..   × (IS NIL)
    .. ⋅ T1 ×1
    ..
    ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-SUCCESS 0.000s ×1>
    ```
    
    This is equivalent to `(WITH-FAILURE-EXPECTED () ...)`. To make
    result failures expected but result successes unexpected:
    
    ```
    (let ((*debug* nil))
      (with-expected-outcome ('(or (and result failure) (and verdict success)))
        (with-test (t1)
          (is t)
          (is nil))))
    .. T1
    ..   ⊡ (IS T)
    ..   × (IS NIL)
    .. ⋅ T1 ⊡1 ×1
    ..
    ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-SUCCESS 0.000s ⊡1 ×1>
    ```
    
    This is equivalent to `(WITH-FAILURE-EXPECTED ('FAILURE) ...)`. The
    final example leaves result failures unexpected but makes both
    verdict successes and failures expected:
    
    ```
    (let ((*debug* nil))
      (with-expected-outcome ('(or (and result success) verdict))
        (with-test (t1)
          (is nil))))
    .. T1
    ..   ⊠ (IS NIL)
    .. × T1 ⊠1
    ..
    ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-FAILURE 0.004s ⊠1>
    ```


<a id='x-28TRY-3AWITH-FAILURE-EXPECTED-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-FAILURE-EXPECTED** *(&OPTIONAL (RESULT-EXPECTED-TYPE T) (VERDICT-EXPECTED-TYPE ''SUCCESS)) &BODY BODY*

    A convenience macro on top of [`WITH-EXPECTED-OUTCOME`][ab7a],
    `WITH-FAILURE-EXPECTED` expects [`VERDICT`][5976]s to have `VERDICT-EXPECTED-TYPE`
    and [`RESULT`][3091]s to have `RESULT-EXPECTED-TYPE`. A simple
    `(WITH-FAILURE-EXPECTED () ...)` makes all `RESULT` [`SUCCESS`][440d]es and
    [`FAILURE`][ea4a]s [`EXPECTED`][7694]. `(WITH-FAILURE-EXPECTED ('FAILURE) ..)` expects
    `FAILURE`s only, and any `SUCCESS`es will be [`UNEXPECTED`][077b].

<a id='x-28TRY-3AWITH-SKIP-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-SKIP** *(&OPTIONAL (SKIP T)) &BODY BODY*

    `WITH-SKIP` skips checks and trials. It forces an immediate
    [`SKIP-TRIAL`][0982] whenever a trial is started (which turns into a
    [`VERDICT-SKIP`][24b5]) and makes checks (without intervening trials, of
    course) evaluate normally but signal [`RESULT-SKIP`][989e]. `SKIP` is `NIL`
    cancels the effect of any enclosing `WITH-SKIP` with `SKIP` true.

<a id='x-28TRY-3A-40TRY-2FOUTCOME-RESTARTS-20MGL-PAX-3ASECTION-29'></a>

#### 4.6.1 Outcome Restarts

<a id='x-28TRY-3AFORCE-EXPECTED-SUCCESS-20FUNCTION-29'></a>

- [function] **FORCE-EXPECTED-SUCCESS** *&OPTIONAL CONDITION*

    Change the type of the [`OUTCOME`][a306] being signalled to [`EXPECTED`][7694] and
    [`SUCCESS`][440d]. If the original condition is a [`RESULT`][3091], then this will be
    [`EXPECTED-RESULT-SUCCESS`][1c37], if it is a [`VERDICT`][5976], then
    [`EXPECTED-VERDICT-SUCCESS`][1431].

<a id='x-28TRY-3AFORCE-UNEXPECTED-SUCCESS-20FUNCTION-29'></a>

- [function] **FORCE-UNEXPECTED-SUCCESS** *&OPTIONAL CONDITION*

    Change the type of [`OUTCOME`][a306] being signalled to [`UNEXPECTED`][077b] and
    [`SUCCESS`][440d].

<a id='x-28TRY-3AFORCE-EXPECTED-FAILURE-20FUNCTION-29'></a>

- [function] **FORCE-EXPECTED-FAILURE** *&OPTIONAL CONDITION*

    Change the type of [`OUTCOME`][a306] being signalled to [`EXPECTED`][7694] and
    [`FAILURE`][ea4a].

<a id='x-28TRY-3AFORCE-UNEXPECTED-FAILURE-20FUNCTION-29'></a>

- [function] **FORCE-UNEXPECTED-FAILURE** *&OPTIONAL CONDITION*

    Change the type of [`OUTCOME`][a306] being signalled to [`UNEXPECTED`][077b] and
    [`FAILURE`][ea4a].

<a id='x-28TRY-3A-40TRY-2FCHECKS-20MGL-PAX-3ASECTION-29'></a>

#### 4.6.2 Checks

Checks are like `CL:ASSERT`s, they check whether some condition holds
and signal an [`OUTCOME`][a306]. The outcome signalled for checks is a
subclass of [`RESULT`][3091].

Take, for example, `(IS (= X 5))`. Depending on whether `X` is
indeed 5, some kind of `RESULT` [`SUCCESS`][440d] or [`FAILURE`][ea4a] will be signalled.
[`WITH-EXPECTED-OUTCOME`][ab7a] determines whether it's [`EXPECTED`][7694] or
[`UNEXPECTED`][077b], and we have one of [`EXPECTED-RESULT-SUCCESS`][1c37],
[`UNEXPECTED-RESULT-SUCCESS`][87a0], [`EXPECTED-RESULT-FAILURE`][16a7],
[`UNEXPECTED-RESULT-FAILURE`][8ce0] to signal. Furthermore, if [`WITH-SKIP`][cd54] is in
effect, then [`RESULT-SKIP`][989e] is signalled.

The result is signalled with `#'SIGNAL` if it is a [`PASS`][338d], else it's
signalled with `#'ERROR`. This distinction matters only if the event
is not handled, which is never the case in a [`TRIAL`][9fc3]. Standalone
checks though - those that are not enclosed by a trial - invoke the
debugger on `RESULT`s which are not of type `PASS`.

The signalled `RESULT` is not final until [`RECORD-EVENT`][318d] is invoked on
it, and it can be changed with the [Outcome Restarts][14a6] and the
[Check Restarts][dbd2].

<a id='x-28TRY-3ARESULT-20CONDITION-29'></a>

- [condition] **RESULT** *LEAF OUTCOME*

<a id='x-28TRY-3AEXPECTED-RESULT-SUCCESS-20CONDITION-29'></a>

- [condition] **EXPECTED-RESULT-SUCCESS** *EXPECTED RESULT SUCCESS*

<a id='x-28TRY-3AUNEXPECTED-RESULT-SUCCESS-20CONDITION-29'></a>

- [condition] **UNEXPECTED-RESULT-SUCCESS** *UNEXPECTED RESULT SUCCESS*

<a id='x-28TRY-3AEXPECTED-RESULT-FAILURE-20CONDITION-29'></a>

- [condition] **EXPECTED-RESULT-FAILURE** *EXPECTED RESULT FAILURE*

<a id='x-28TRY-3AUNEXPECTED-RESULT-FAILURE-20CONDITION-29'></a>

- [condition] **UNEXPECTED-RESULT-FAILURE** *UNEXPECTED RESULT FAILURE*

<a id='x-28TRY-3ARESULT-SKIP-20CONDITION-29'></a>

- [condition] **RESULT-SKIP** *RESULT SKIP*

<a id='x-28TRY-3ARESULT-ABORT-2A-20CONDITION-29'></a>

- [condition] **RESULT-ABORT\*** *RESULT ABORT\* DISMISSAL*

<a id='x-28TRY-3A-40TRY-2FCHECK-RESTARTS-20MGL-PAX-3ASECTION-29'></a>

##### 4.6.2.1 Check Restarts

<a id='x-28TRY-3AABORT-CHECK-20FUNCTION-29'></a>

- [function] **ABORT-CHECK** *&OPTIONAL CONDITION*

    Change the [`OUTCOME`][a306] of the check being signalled to [`RESULT-ABORT*`][4c47].
    `RESULT-ABORT*`, being `(NOT PASS)`, will cause the check to return
    `NIL` if [`RECORD-EVENT`][318d] is invoked on it.

<a id='x-28TRY-3ASKIP-CHECK-20FUNCTION-29'></a>

- [function] **SKIP-CHECK** *&OPTIONAL CONDITION*

    Change the [`OUTCOME`][a306] of the check being signalled to [`RESULT-SKIP`][989e].
    `RESULT-SKIP`, being a [`PASS`][338d], will cause the check to return `T` if
    `CONTINUE` or [`RECORD-EVENT`][318d] is invoked on it.

<a id='x-28TRY-3ARETRY-CHECK-20FUNCTION-29'></a>

- [function] **RETRY-CHECK** *&OPTIONAL CONDITION*

    Initiate a [non-local exit][c4d2] to go reevaluate the forms wrapped by
    the check without signalling an [`OUTCOME`][a306].

<a id='x-28TRY-3A-40TRY-2FTRIALS-20MGL-PAX-3ASECTION-29'></a>

#### 4.6.3 Trials

<a id='x-28TRY-3ATRIAL-20TYPE-29'></a>

- [type] **TRIAL**

    Trials are records of calls to tests (see
    [Counting Events][3c27], [Collecting Events][0a38]). Their behaviour as [funcallable instance][de86]s
    is explained in [Rerunning Trials][76af].
    
    There are three ways to acquire a `TRIAL` object: by calling
    [`CURRENT-TRIAL`][e542], through the lexical binding of the symbol that names
    the test or through the return value of a test:
    
    ```
    (deftest xxx ()
      (prin1 xxx))
    
    (xxx)
    .. #<TRIAL (XXX) RUNNING>
    ==> #<TRIAL (XXX) EXPECTED-SUCCESS 0.000s>
    ```
    
    `WITH-TRIAL` can also provide access to its `TRIAL`:
    
    ```
    (with-test (t0)
      (prin1 t0))
    .. #<TRIAL (WITH-TEST (T0)) RUNNING>
    ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s>
    ```
    
    `TRIAL`s are not to be instantiated by client code.

<a id='x-28TRY-3ACURRENT-TRIAL-20FUNCTION-29'></a>

- [function] **CURRENT-TRIAL** 

    [`TRIAL`][9fc3]s, like the calls to tests they stand for, nest. `CURRENT-TRIAL`
    returns the innermost trial. If there is no currently running test,
    then an error is signalled. The returned trial is [`RUNNINGP`][c773].

<a id='x-28TRY-3A-40TRY-2FTRIAL-EVENTS-20MGL-PAX-3ASECTION-29'></a>

##### 4.6.3.1 Trial Events

<a id='x-28TRY-3ATRIAL-EVENT-20CONDITION-29'></a>

- [condition] **TRIAL-EVENT** *EVENT*

    A `TRIAL-EVENT` is either a [`TRIAL-START`][a231] or a
    [`VERDICT`][5976].

<a id='x-28TRY-3ATRIAL-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-EVENT-29-29'></a>

- [reader] **TRIAL** *TRIAL-EVENT* *(:TRIAL)*

<a id='x-28TRY-3ATRIAL-START-20CONDITION-29'></a>

- [condition] **TRIAL-START** *TRIAL-EVENT*

    `TRIAL-START` is signalled when a test function
    (see [Tests][51bb]) is entered and a [`TRIAL`][9fc3] is started, it is already
    the [`CURRENT-TRIAL`][e542], and the [Trial Restarts][dbf0] are available. It is
    also signalled when a trial is retried:
    
    ```
    (let ((*print* nil))
      (with-test ()
        (handler-bind ((trial-start (lambda (c)
                                      (format t "TRIAL-START for ~S retry#~S~%"
                                              (test-name (trial c))
                                              (n-retries (trial c))))))
          (with-test (this)
            (when (zerop (random 2))
              (retry-trial))))))
    .. TRIAL-START for THIS retry#0
    .. TRIAL-START for THIS retry#1
    .. TRIAL-START for THIS retry#2
    ..
    ```
    
    The matching of `TRIAL-START` events is less straightforward than that
    of other [`EVENT`][6ded]s.
    
    - When a `TRIAL-START` event matches the `COLLECT` type (see
      [Collecting Events][0a38]), its [`TRIAL`][6a27] is collected.
    
    - Similarly, when a `TRIAL-START` matches the `PRINT` type (see
      [Printing Events][53fc]), it is printed immediately, and its trial's [`VERDICT`][5976]
      will be printed too regardless of whether it matches `PRINT`. If
      `TRIAL-START` does not match `PRINT`, it may still be printed if for
      example [`*PRINT-PARENT*`][85d7] requires it.
    
    - When a `TRIAL-START` matches the `RERUN` type (see [Rerunning Trials][76af]), its
      [`TRIAL`][6a27] may be rerun.
    
    - Also, see [`WITH-SKIP`][cd54].


<a id='x-28TRY-3AVERDICT-20CONDITION-29'></a>

- [condition] **VERDICT** *TRIAL-EVENT OUTCOME*

    A `VERDICT` is the [`OUTCOME`][a306] of a [`TRIAL`][9fc3]. It is one of
    `{EXPECTED,UNEXPECTED}-VERDICT-{SUCCESS,FAILURE}`, [`VERDICT-SKIP`][24b5] and
    [`VERDICT-ABORT*`][ca88]. Regarding how the verdict type is determined, see
    [Trial Verdicts][ea26].
    
    Verdicts are signalled while their [`TRIAL`][6a27] is
    still the [`CURRENT-TRIAL`][e542], and [Trial Restarts][dbf0] are still
    available.
    
    ```
    (try (lambda ()
           (handler-bind (((and verdict failure) #'retry-trial))
             (with-test (this)
               (is (zerop (random 2)))))))
    .. (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>)
    ..   THIS
    ..     ⊠ (IS (ZEROP #1=(RANDOM 2)))
    ..       where
    ..         #1# = 1
    ..   THIS retry #1
    ..     ⋅ (IS (ZEROP (RANDOM 2)))
    ..   ⋅ THIS ⋅1
    .. ⋅ (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>) ⋅1
    ..
    ==> #<TRIAL (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>) EXPECTED-SUCCESS 0.000s ⋅1>
    ```


<a id='x-28TRY-3AEXPECTED-VERDICT-SUCCESS-20CONDITION-29'></a>

- [condition] **EXPECTED-VERDICT-SUCCESS** *EXPECTED VERDICT SUCCESS*

<a id='x-28TRY-3AUNEXPECTED-VERDICT-SUCCESS-20CONDITION-29'></a>

- [condition] **UNEXPECTED-VERDICT-SUCCESS** *UNEXPECTED VERDICT SUCCESS*

<a id='x-28TRY-3AEXPECTED-VERDICT-FAILURE-20CONDITION-29'></a>

- [condition] **EXPECTED-VERDICT-FAILURE** *EXPECTED VERDICT FAILURE*

<a id='x-28TRY-3AUNEXPECTED-VERDICT-FAILURE-20CONDITION-29'></a>

- [condition] **UNEXPECTED-VERDICT-FAILURE** *UNEXPECTED VERDICT FAILURE*

<a id='x-28TRY-3AVERDICT-SKIP-20CONDITION-29'></a>

- [condition] **VERDICT-SKIP** *VERDICT SKIP*

<a id='x-28TRY-3AVERDICT-ABORT-2A-20CONDITION-29'></a>

- [condition] **VERDICT-ABORT\*** *VERDICT ABORT\* DISMISSAL*

<a id='x-28TRY-3A-40TRY-2FTRIAL-VERDICTS-20MGL-PAX-3ASECTION-29'></a>

##### 4.6.3.2 Trial Verdicts

When a trial finished, a [`VERDICT`][5976] is signalled. The verdict's type
is determined as follows.

- It is a [`VERDICT-SKIP`][24b5] if

    - [`SKIP-TRIAL`][0982] was called on the trial, or

    - [`ABORT-TRIAL`][c705], `SKIP-TRIAL`, or [`RETRY-TRIAL`][93e2] was called on an
      enclosing trial, and

    - these were not overruled by a later `ABORT-TRIAL` or `RETRY-TRIAL`
      on the trial.

- It is a [`VERDICT-ABORT*`][ca88] if `ABORT-TRIAL` was called on the trial, and
  it wasn't overruled by a later `SKIP-TRIAL` or `RETRY-TRIAL`.

- If all children (including those not collected in [`CHILDREN`][e616]) of the
  trial [`PASS`][338d], then the verdict will be a [`SUCCESS`][440d], else it will be a
  [`FAILURE`][ea4a].

- Subject to the [`WITH-EXPECTED-OUTCOME`][ab7a] in effect,
  `{EXPECTED,UNEXPECTED}-VERDICT-{SUCCESS,FAILURE}` is the type of
  the verdict which will be signalled.

The verdict of this type is signalled, but its type can be changed
by the [Outcome Restarts][14a6] or the [Trial Restarts][dbf0] before
[`RECORD-EVENT`][318d] is invoked on it.

<a id='x-28TRY-3AVERDICT-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29'></a>

- [reader] **VERDICT** *TRIAL* *(= NIL)*

    The [`VERDICT`][5976] [`EVENT`][6ded] signalled when this
    `TRIAL` finished or `NIL` if it has not finished yet.

<a id='x-28TRY-3ARUNNINGP-20FUNCTION-29'></a>

- [function] **RUNNINGP** *TRIAL*

    See if the function call associated with `TRIAL` has not returned yet.
    Trials that are not running have a [`VERDICT`][5976] and are said to be
    finished.

<a id='x-28TRY-3APASSEDP-20FUNCTION-29'></a>

- [function] **PASSEDP** *TRIAL*

    See if `TRIAL` has finished and its [`VERDICT`][4cbe] is a
    [`PASS`][338d].

<a id='x-28TRY-3AFAILEDP-20FUNCTION-29'></a>

- [function] **FAILEDP** *TRIAL*

    See if `TRIAL` has finished and its [`VERDICT`][4cbe] is a
    [`FAIL`][7831].

<a id='x-28TRY-3A-40TRY-2FTRIAL-RESTARTS-20MGL-PAX-3ASECTION-29'></a>

##### 4.6.3.3 Trial Restarts

There are three restarts available for manipulating running
trials: [`ABORT-TRIAL`][c705], [`SKIP-TRIAL`][0982], and [`RETRY-TRIAL`][93e2]. They may be
invoked programatically or from the debugger. `ABORT-TRIAL` is also
invoked by [`TRY`][7a62] when encountering [`UNHANDLED-ERROR`][e65b].

The functions below invoke one of these restarts associated with a
[`TRIAL`][9fc3]. It is an error to call them on trials that are not [`RUNNINGP`][c773],
but they may be called on trials other than the [`CURRENT-TRIAL`][e542]. In
that case, any intervening trials are skipped.

```
;; Skipped trials are marked with '-' in the output.
(with-test (outer)
  (with-test (inner)
    (is t)
    (skip-trial nil outer)))
.. OUTER
..   INNER
..     ⋅ (IS T)
..   - INNER ⋅1
.. - OUTER ⋅1
..
==> #<TRIAL (WITH-TEST (OUTER)) SKIP 0.000s ⋅1>
```

Furthermore, all three restarts initiate a [non-local exit][c4d2] to return
from the trial. If during the unwinding of the stack, the
non-local-exit is cancelled (see [cancelled non-local exit][d65b]), the appropriate
restart will be invoked upon returning from the trial. In the
following example, the non-local exit from a skip is cancelled by a
`THROW`.

```
(with-test (some-test)
  (catch 'foo
    (unwind-protect
         (skip-trial)
      (throw 'foo nil)))
  (is t :msg "check after skip"))
.. SOME-TEST
..   ⋅ check after skip
.. - SOME-TEST ⋅1
..
==> #<TRIAL (WITH-TEST (SOME-TEST)) SKIP 0.000s ⋅1>
```

In the next example, the non-local exit from a skip is cancelled by
an `ERROR`, which triggers an `ABORT-TRIAL`.

```
(let ((*debug* nil)
      (*describe* nil))
  (with-test (foo)
    (unwind-protect
         (skip-trial)
      (error "xxx"))))
.. FOO
..   ⊟ "xxx" (SIMPLE-ERROR)
.. ⊟ FOO ⊟1
..
==> #<TRIAL (WITH-TEST (FOO)) ABORT* 0.000s ⊟1>
```

All three restarts may be invoked on any [`EVENT`][6ded], including the
trial's own [`TRIAL-START`][a231] and [`VERDICT`][5976]. If `CONDITION` is an `EVENT`
(`RETRY-TRIAL` has a special case here), they also record it (as in
[`RECORD-EVENT`][318d]) to ensure that when they handle an `EVENT` in the
debugger or programatically that event is not dropped.

<a id='x-28TRY-3AABORT-TRIAL-20FUNCTION-29'></a>

- [function] **ABORT-TRIAL** *&OPTIONAL CONDITION (TRIAL (CURRENT-TRIAL))*

    Invoke the `ABORT-TRIAL` restart of a [`RUNNINGP`][c773] `TRIAL`.
    
    When `CONDITION` is a [`VERDICT`][5976] for `TRIAL`, `ABORT-TRIAL` signals a new
    verdict of type VERDICT-ABORT*. This behavior is similar to that of
    [`ABORT-CHECK`][eb00]. Else, the `ABORT-TRIAL` restart may record `CONDITION`,
    then it initiates a [non-local exit][c4d2] to return from the test function
    with VERDICT-ABORT*. If during the unwinding [`SKIP-TRIAL`][0982] or
    [`RETRY-TRIAL`][93e2] is called, then the abort is cancelled.
    
    Since [`ABORT*`][cca5] is an [`UNEXPECTED`][077b] [`EVENT`][6ded], `ABORT-TRIAL` is rarely used
    programatically. Signalling any error in a trial that's not caught
    before the trial's handler catches it will get turned into an
    [`UNHANDLED-ERROR`][e65b], and [`TRY`][7a62] will invoke `ABORT-TRIAL` with it. Thus,
    instead of invoking `ABORT-TRIAL` directly, signalling an error will
    often suffice.

<a id='x-28TRY-3ASKIP-TRIAL-20FUNCTION-29'></a>

- [function] **SKIP-TRIAL** *&OPTIONAL CONDITION (TRIAL (CURRENT-TRIAL))*

    Invoke the `SKIP-TRIAL` restart of a [`RUNNINGP`][c773] `TRIAL`.
    
    When `CONDITION` is a [`VERDICT`][5976] for `TRIAL`, `SKIP-TRIAL` signals a new
    verdict of type [`VERDICT-SKIP`][24b5]. This behavior is similar to that of
    [`SKIP-CHECK`][44ee]. Else, the `SKIP-TRIAL` restart may record `CONDITION`, then
    it initiates a [non-local exit][c4d2] to return from the test function with
    `VERDICT-SKIP`. If during the unwinding [`ABORT-TRIAL`][c705] or [`RETRY-TRIAL`][93e2] is
    called, then the skip is cancelled.
    
    ```
    (with-test (skipped)
      (handler-bind ((unexpected-result-failure #'skip-trial))
        (is nil)))
    .. SKIPPED
    ..   ⊠ (IS NIL)
    .. - SKIPPED ⊠1
    ..
    ==> #<TRIAL (WITH-TEST (SKIPPED)) SKIP 0.000s ⊠1>
    ```
    
    Invoking `SKIP-TRIAL` on the `TRIAL`'s own [`TRIAL-START`][a231] skips the trial
    being started.
    
    ```
    (let ((*print* '(or outcome leaf)))
      (with-test (parent)
        (handler-bind ((trial-start #'skip-trial))
          (with-test (child)
            (is nil)))))
    .. PARENT
    ..   - CHILD
    .. ⋅ PARENT
    ..
    ```


<a id='x-28TRY-3ARETRY-TRIAL-20FUNCTION-29'></a>

- [function] **RETRY-TRIAL** *&OPTIONAL CONDITION (TRIAL (CURRENT-TRIAL))*

    Invoke the `RETRY-TRIAL` restart of [`RUNNINGP`][c773] `TRIAL`. The `RETRY-TRIAL`
    restart may record `CONDITION`, then it initiates a [non-local exit][c4d2] to
    go back to the beginning of the test function. If the non-local exit
    completes, then
    
    - ([`N-RETRIES`][6909] `TRIAL`) is incremented,
    
    - collected results and trials are cleared (see [Collecting Events][0a38]),
    
    - counts are zeroed (see [Counting Events][3c27]), and
    
    - [`TRIAL-START`][a231] is signalled again.
    
    If during the unwinding [`ABORT-TRIAL`][c705] or [`SKIP-TRIAL`][0982] is called, then
    the retry is cancelled.
    
    `CONDITION` (which may be `NIL`) is recorded if it is an [`EVENT`][6ded] but not
    the [`VERDICT`][5976] of `TRIAL`, and the [`RECORD-EVENT`][318d] restart is available.

<a id='x-28TRY-3AN-RETRIES-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29'></a>

- [reader] **N-RETRIES** *TRIAL* *(:N-RETRIES = 0)*

    The number of times this `TRIAL` has
    been retried. See [`RETRY-TRIAL`][93e2].

<a id='x-28TRY-3A-40TRY-2FERRORS-20MGL-PAX-3ASECTION-29'></a>

### 4.7 Errors

<a id='x-28TRY-3AERROR-2A-20CONDITION-29'></a>

- [condition] **ERROR\*** *ABORT\* TRIAL-EVENT LEAF*

    Either [`UNHANDLED-ERROR`][e65b] or [`NLX`][d43d], `ERROR*` causes or
    represents abnormal termination of a [`TRIAL`][9fc3]. [`ABORT-TRIAL`][c705] can be
    called with `ERROR*`s, but there is little need for explicitly doing
    so as [`RECORD-EVENT`][318d], which [`TRY`][7a62] invokes, takes care of this.

<a id='x-28TRY-3ATEST-NAME-20-28MGL-PAX-3AREADER-20TRY-3AERROR-2A-29-29'></a>

- [reader] **TEST-NAME** *ERROR\** *(:TEST-NAME)*

<a id='x-28TRY-3AUNHANDLED-ERROR-20CONDITION-29'></a>

- [condition] **UNHANDLED-ERROR** *ERROR\**

    Signalled when an `CL:ERROR` condition reaches the
    handlers set up [`DEFTEST`][e6a7] or [`WITH-TEST`][af8d], or when their `*DEBUGGER-HOOK*`
    is invoked with a condition that's not an [`EVENT`][6ded].

<a id='x-28TRY-3ANESTED-CONDITION-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29'></a>

- [reader] **NESTED-CONDITION** *UNHANDLED-ERROR* *(:CONDITION = 'NIL)*

<a id='x-28TRY-3ABACKTRACE-OF-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29'></a>

- [reader] **BACKTRACE-OF** *UNHANDLED-ERROR* *(:BACKTRACE = 'NIL)*

<a id='x-28TRY-3ADEBUGGER-INVOKED-P-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29'></a>

- [reader] **DEBUGGER-INVOKED-P** *UNHANDLED-ERROR* *(:DEBUGGER-INVOKED-P = 'NIL)*

<a id='x-28TRY-3A-2AGATHER-BACKTRACE-2A-20VARIABLE-29'></a>

- [variable] **\*GATHER-BACKTRACE\*** *T*

    Capturing the backtrace can be expensive. `*GATHER-BACKTRACE*`
    controls whether [`UNHANDLED-ERROR`][e65b]s shall have their [`BACKTRACE-OF`][e019]
    populated.

<a id='x-28TRY-3ANLX-20CONDITION-29'></a>

- [condition] **NLX** *ERROR\**

    Representing a [non-local exit][c4d2] of unknown origin,
    this is signalled if a [`TRIAL`][9fc3] does not return normally although it
    should have because it was not dismissed (see [`DISMISSAL`][68db], [`SKIP-TRIAL`][0982],
    [`ABORT-TRIAL`][c705]). In this case, there is no `CL:ERROR` associated with the
    event.

<a id='x-28TRY-3A-40TRY-2FCATEGORIES-20MGL-PAX-3ASECTION-29'></a>

### 4.8 Categories

Categories determine how event types are printed and events of
what types are counted together.

The default value of [`*CATEGORIES*`][3d4c] is

```
((abort*             :marker "⊟")
 (unexpected-failure :marker "⊠")
 (unexpected-success :marker "⊡")
 (skip               :marker "-")
 (expected-failure   :marker "×")
 (expected-success   :marker "⋅"))
```

which says that all concrete [`EVENT`][6ded]s that are of type [`ABORT*`][cca5] (i.e.
[`RESULT-ABORT*`][4c47], [`VERDICT-ABORT*`][ca88], [`UNHANDLED-ERROR`][e65b], and [`NLX`][d43d]) are to
be marked with `"⊟"` when printed (see [Printing Events][53fc]). Also, the six
types define six counters for [Counting Events][3c27]. Note that [`UNEXPECTED`][077b]
events have the same marker but squared as their [`EXPECTED`][7694]
counterpart.

<a id='x-28TRY-3A-2ACATEGORIES-2A-20-28VARIABLE-20-22--20see-20above-20--22-29-29'></a>

- [variable] **\*CATEGORIES\*** *"- see above -"*

    A list of of elements like `(TYPE &KEY MARKER)`.
    When [Printing Events][53fc], [Concrete Events][8a03] are printed with the marker of
    the first matching type. When [Counting Events][3c27], the counts associated with
    all matching types are incremented.

<a id='x-28TRY-3AFANCY-STD-CATEGORIES-20FUNCTION-29'></a>

- [function] **FANCY-STD-CATEGORIES** 

    Returns the default value of [`*CATEGORIES*`][3d4c] (see [Categories][f1c6]),
    which contains some fancy Unicode characters.

<a id='x-28TRY-3AASCII-STD-CATEGORIES-20FUNCTION-29'></a>

- [function] **ASCII-STD-CATEGORIES** 

    Returns a value suitable for [`*CATEGORIES*`][3d4c], which uses only ASCII
    characters for the markers.
    
    ```
    '((abort*             :marker "!")
      (unexpected-failure :marker "F")
      (unexpected-success :marker ":")
      (skip               :marker "-")
      (expected-failure   :marker "f")
      (expected-success   :marker "."))
    ```


<a id='x-28TRY-3A-40TRY-2FIS-20MGL-PAX-3ASECTION-29'></a>

## 5 The IS Macro

[`IS`][ff02] is the most fundamental one among [Checks][3e2d], on which all
the others are built, and it is a replacement for `CL:ASSERT` that can
capture values of subforms to provide context to failures:

```
(is (= (1+ 5) 0))

debugger invoked on a TRY:UNEXPECTED-FAILURE:
  UNEXPECTED-FAILURE in check:
    (IS (= #1=(1+ 5) 0))
  where
    #1# = 6
```

`IS` automatically captures values of arguments to functions like `1+`
in the above example. Values of other interesting subforms can be
explicitly requested to be captured. `IS` supports capturing multiple
values and can be taught how to deal with macros. The combination of
these features allows [`MATCH-VALUES`][1f28] to be implementable as tiny
extension:

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

`IS` is flexible enough that all other checks ([`SIGNALS`][6ebb], [`SIGNALS-NOT`][1a75],
[`INVOKES-DEBUGGER`][9539], [`INVOKES-DEBUGGER-NOT`][b12f], [`FAILS`][20b7], and [`IN-TIME`][6249] are built
on top of it.

<a id='x-28TRY-3AIS-20MGL-PAX-3AMACRO-29'></a>

- [macro] **IS** *FORM &KEY MSG CTX (CAPTURE T) (PRINT-CAPTURES T) (RETRY T)*

    Evaluate `FORM` and signal a [`RESULT`][3091] [`SUCCESS`][440d] if its first return
    value is not `NIL`, else signal a `RESULT` [`FAILURE`][ea4a] (see [Outcomes][95c4]).
    `IS` returns normally if
    
    - the [`RECORD-EVENT`][318d] restart is invoked (available when running in a
      trial), or
    
    - the `CONTINUE` restart is invoked (available when not running in a
      trial), or
    
    - the signalled `RESULT` condition is not handled (possible only when
      not running in a trial, and the result is a [`PASS`][338d]).
    
    The return value of `IS` is `T` if the last condition signalled is a
    `PASS`, and `NIL` otherwise.
    
    `MSG` and `CTX` are [Format Specifier Forms][22e6]. `MSG` prints a
    description of the check being made, which is by default the whole
    `IS` form. Due to how conditions are printed, `MSG` says what the
    desired outcome is, and `CTX` provides information about the
    evaluation.
    
    ```
    (is (equal (prin1-to-string 'hello) "hello")
        :msg "Symbols are replacements for strings." 
        :ctx ("*PACKAGE* is ~S and *PRINT-CASE* is ~S~%"
              *package* *print-case*))
    
    UNEXPECTED-FAILURE in check:
      Symbols are replacements for strings.
    where
      (PRIN1-TO-STRING 'HELLO) = "HELLO"
    *PACKAGE* is #<PACKAGE "TRY"> and *PRINT-CASE* is :UPCASE
    ```
    
    If `CAPTURE` is true, the value(s) of some subforms of `FORM` may be
    automatically recorded in the condition and also made available for
    `CTX` via [`*IS-CAPTURES*`][74b0]. See [Captures][a03c] for more.
    
    If `PRINT-CAPTURES` is true, the captures made are printed when the
    `RESULT` condition is displayed in the debugger or `DESCRIBE`d (see
    [Printing Events][53fc]). This is the `where (PRIN1-TO-STRING 'HELLO) = "HELLO"`
    part above. If `PRINT-CAPTURES` is `NIL`, the captures are still
    available in `*IS-CAPTURES*` for writing custom `CTX` messages.
    
    If `RETRY` is true, then the [`RETRY-CHECK`][0d2f] restart evaluates `FORM` again
    and signals a new `RESULT`. If `RETRY` is `NIL`, then the `RETRY-CHECK`
    restart returns `:RETRY`, which allows complex checks such as [`SIGNALS`][6ebb]
    to implement their own retry mechanism.

<a id='x-28TRY-3A-2AIS-FORM-2A-20VARIABLE-29'></a>

- [variable] **\*IS-FORM\*** *"-unbound-"*

    [`IS`][ff02] binds this to its `FORM` argument for `CTX` and `MSG`.

<a id='x-28TRY-3A-2AIS-CAPTURES-2A-20VARIABLE-29'></a>

- [variable] **\*IS-CAPTURES\*** *"-unbound-"*

    Captures made during an [`IS`][ff02] evaluation are made available for
    `CTX` via `*IS-CAPTURES*`.

<a id='x-28TRY-3A-40TRY-2FFORMAT-SPECIFIER-FORMS-20MGL-PAX-3ASECTION-29'></a>

### 5.1 Format Specifier Forms

A format specifier form is a Lisp form, typically an argument to
macro, standing for the `FORMAT-CONTROL` and `FORMAT-ARGS` arguments to
the `FORMAT` function.

It may be a constant string:

```
(is nil :msg "FORMAT-CONTROL~%with no args.")

UNEXPECTED-FAILURE in check:
  FORMAT-CONTROL
  with no args.
```

It may be a list whose first element is a constant string, and the
rest are the format arguments to be evaluated:

```
(is nil :msg ("Implicit LIST ~A." "form"))

UNEXPECTED-FAILURE in check:
  Implicit LIST form.
```

Or it may be a form that evaluates to a list like `(FORMAT-CONTROL
&REST FORMAT-ARGS)`:

```
(is nil :msg (list "Full ~A." "form"))

UNEXPECTED-FAILURE in check:
  Full form.
```

Finally, it may evaluate to `NIL`, in which case some context specific
default is implied.

<a id='x-28TRY-3ACANONICALIZE-FORMAT-SPECIFIER-FORM-20FUNCTION-29'></a>

- [function] **CANONICALIZE-FORMAT-SPECIFIER-FORM** *FORM*

    Ensure that the format specifier form `FORM` is in its full form.

<a id='x-28TRY-3A-40TRY-2FCAPTURES-20MGL-PAX-3ASECTION-29'></a>

### 5.2 Captures

During the evaluation of the `FORM` argument of [`IS`][ff02], evaluation of any
form (e.g. a subform of `FORM`) may be recorded, which are called
captures.

<a id='x-28TRY-3A-40TRY-2FAUTOMATIC-CAPTURES-20MGL-PAX-3ASECTION-29'></a>

#### 5.2.1 Automatic Captures

[`IS`][ff02] automatically captures some subforms of `FORM` that are likely
to be informative. In particular, if `FORM` is a function call, then
non-constant arguments are automatically captured:

```
(is (= 3 (1+ 2) (- 4 3)))

UNEXPECTED-FAILURE in check:
  (IS (= 3 #1=(1+ 2) #2=(- 4 3)))
where
  #1# = 3
  #2# = 1
```

By default, automatic captures are not made for subforms deeper in
`FORM`, except for when `FORM` is a call to `NULL`, `ENDP` and `NOT`:

```
(is (null (find (1+ 1) '(1 2 3))))

UNEXPECTED-FAILURE in check:
  (IS (NULL #1=(FIND #2=(1+ 1) '(1 2 3))))
where
  #2# = 2
  #1# = 2
```

```
(is (endp (member (1+ 1) '(1 2 3))))

UNEXPECTED-FAILURE in check:
  (IS (ENDP #1=(MEMBER #2=(1+ 1) '(1 2 3))))
where
  #2# = 2
  #1# = (2 3)
```

Note that the argument of `NOT` is not captured as it is assumed to be
`NIL` or `T`. If that's not true, use `NULL`.

```
(is (not (equal (1+ 5) 6)))

UNEXPECTED-FAILURE in check:
  (IS (NOT (EQUAL #1=(1+ 5) 6)))
where
  #1# = 6
```

Other automatic captures are discussed with the relevant
functionality such as [`MATCH-VALUES`][1f28].

<a id='x-28TRY-3A-40TRY-2FWRITING-AUTOMATIC-CAPTURE-RULES-20MGL-PAX-3ASECTION-29'></a>

##### 5.2.1.1 Writing Automatic Capture Rules

<a id='x-28TRY-3ASUB-20CLASS-29'></a>

- [class] **SUB** *STRUCTURE-OBJECT*

    A `SUB` (short for substitution) says that in the original form [`IS`][ff02] is
    checking, a `SUBFORM` was substituted (by `SUBSTITUTE-IS-FORM`) with
    `VAR` (if `VALUESP` is `NIL`) or with (`VALUES-LIST` `VAR`) if `VALUESP` is
    true. Conversely, `VAR` is to be bound to the evaluated `NEW-FORM` if
    `VALUESP` is `NIL`, and to (`MULTIPLE-VALUE-LIST` `FORM`) if `VALUESP`.
    `NEW-FORM` is often `EQ` to `SUBFORM`, but it may be different, which is
    the case when further substitutions are made within a substitution.

<a id='x-28TRY-3AMAKE-SUB-20FUNCTION-29'></a>

- [function] **MAKE-SUB** *VAR SUBFORM NEW-FORM VALUESP*

<a id='x-28TRY-3ASUB-VAR-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29'></a>

- [structure-accessor] **SUB-VAR**

<a id='x-28TRY-3ASUB-SUBFORM-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29'></a>

- [structure-accessor] **SUB-SUBFORM**

<a id='x-28TRY-3ASUB-NEW-FORM-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29'></a>

- [structure-accessor] **SUB-NEW-FORM**

<a id='x-28TRY-3ASUB-VALUESP-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29'></a>

- [structure-accessor] **SUB-VALUESP**

<a id='x-28TRY-3ASUBSTITUTE-IS-LIST-FORM-20GENERIC-FUNCTION-29'></a>

- [generic-function] **SUBSTITUTE-IS-LIST-FORM** *FIRST FORM ENV*

    In the list `FORM`, whose `CAR` is `FIRST`, substitute
    subexpressions of interest with a `GENSYM` and return the new form. As
    the second value, return a list of `SUBs`.
    
    For example, consider `(IS (FIND (FOO) LIST))`. When
    `SUBSTITUTE-IS-LIST-FORM` is invoked on `(FIND (FOO) LIST)`, it
    substitutes each argument of `FIND` with a variable, returning the new
    form `(FIND TEMP1 TEMP2)` and the list of two
    substitutions `((TEMP2 (FOO) (FOO) NIL) (TEMP3 LIST LIST NIL))`.
    This allows the original form to be rewritten as
    
    ```
    (let* ((temp1 (foo))
           (temp2 list))
      (find temp1 temp2))
    ```
    
    TEMP1 and TEMP2 may then be reported in the [`OUTCOME`][a306] condition
    signalled by [`IS`][ff02] like this:
    
        The following check failed:
          (is (find #1=(foo) #2=list))
        where
          #1# = <return-value-of-foo>
          #2# = <value-of-variable-list>


<a id='x-28TRY-3A-40TRY-2FEXPLICIT-CAPTURES-20MGL-PAX-3ASECTION-29'></a>

#### 5.2.2 Explicit Captures

In addition to automatic captures, which are prescribed by
rewriting rules (see [Writing Automatic Capture Rules][afcd]),
explicit, ad-hoc captures can also be made.

```
(is (let ((x 1))
      (= (capture x) 2)))

UNEXPECTED-FAILURE in check:
  (IS
   (LET ((X 1))
     (= (CAPTURE X) 2)))
where
  X = 1
```

If [`CAPTURE`][4f4d] showing up in the form that [`IS`][ff02] print is undesirable, then
[`%`][d94b] may be used instead:

```
(is (let ((x 1))
      (= (% x) 2)))

UNEXPECTED-FAILURE in check:
  (IS
   (LET ((X 1))
     (= X 2)))
where
  X = 1
```

Multiple values may be captured with [`CAPTURE-VALUES`][67de] and its
secretive counterpart [`%%`][b858]:

```
(is (= (%% (values 1 2)) 2))

UNEXPECTED-FAILURE in check:
  (IS (= #1=(VALUES 1 2) 2))
where
  #1# == 1
         2
```

where printing `==` instead of `=` indicates that this is a multiple
value capture.

<a id='x-28TRY-3ACAPTURE-20MGL-PAX-3AMACRO-29'></a>

- [macro] **CAPTURE** *FORM*

    Evaluate `FORM`, record its primary return value if within the
    dynamic extent of an [`IS`][ff02] evaluation, and finally return that value.
    If `CAPTURE` is used within the lexical scope of `IS`, then `CAPTURE`
    itself will show up in the form that the default `MSG` prints. Thus it
    is recommended to use the equivalent `MACROLET` [`%`][d94b] in the lexical
    scope as `%` is removed before printing.

<a id='x-28TRY-3ACAPTURE-VALUES-20MGL-PAX-3AMACRO-29'></a>

- [macro] **CAPTURE-VALUES** *FORM*

    Like `CAPTURE-VALUES`, but record and return all values returned by
    `FORM`. It is recommended to use the equivalent `MACROLET` [`%%`][b858] in the
    lexical scope as `%%` is removed before printing.

<a id='x-28TRY-3A-25-20MACROLET-29'></a>

- [macrolet] **%** *FORM*

    An alias for [`CAPTURE`][4f4d] in the lexical scope of [`IS`][ff02]. Removed from the
    `IS` form when printed.

<a id='x-28TRY-3A-25-25-20MACROLET-29'></a>

- [macrolet] **%%** *FORM*

    An alias for [`CAPTURE-VALUES`][67de] in the lexical scope of [`IS`][ff02]. Removed
    from the `IS` form when printed.

<a id='x-28TRY-3A-40TRY-2FCHECK-LIBRARY-20MGL-PAX-3ASECTION-29'></a>

## 6 Check Library

In the following, various checks built on top of [`IS`][ff02] are described.
Many of them share a number of arguments, which are described here.

- `ON-RETURN` is a boolean that determines whether the check in a
  macro that wraps `BODY` is made when `BODY` returns normally.

- `ON-NLX` is a boolean that determines whether the check in a macro
  that wraps `BODY` is made when `BODY` performs a [non-local exit][c4d2].

- `MSG` and `CTX` are [Format Specifier Forms][22e6] as in `IS`.

- `NAME` may be provided so that it is printed (with `PRIN1`) instead of
  `BODY` in `MSG`.


<a id='x-28TRY-3A-40TRY-2FCHECKING-CONDITIONS-20MGL-PAX-3ASECTION-29'></a>

### 6.1 Checking Conditions

The macros [`SIGNALS`][6ebb], [`SIGNALS-NOT`][1a75], [`INVOKES-DEBUGGER`][9539], and
[`INVOKES-DEBUGGER-NOT`][b12f] all check whether a condition of a given type,
possibly also matching a predicate, was signalled. In addition to
those already described in [Check Library][4fbb], these macros share a
number of arguments.

Matching conditions are those that are of type `CONDITION-TYPE` (not
evaluated) and satisfy the predicate `PRED`.

When `PRED` is `NIL`, it always matches. When it is a string, then it
matches if it is a substring of the printed representation of the
condition being handled (by `PRINC` under `WITH-STANDARD-IO-SYNTAX`).
When it is a function, it matches if it returns true when called
with the condition as its argument.

The check is performed in the cleanup form of an `UNWIND-PROTECT`
around `BODY`.

`HANDLER` is called when a matching condition is found. It can be a
function, `T`, or `NIL`. When it is a function, it is called from the
condition handler (`SIGNALS` and `SIGNALS-NOT`) or the debugger
hook (invokes-debugger and `INVOKES-DEBUGGER-NOT`) with the matching
condition. `HANDLER` may perform a [non-local exit][c4d2]. When `HANDLER` is `T`,
the matching condition is handled by performing a non-local exit to
just outside `BODY`. If the exit completes, `BODY` is treated as if it
had returned normally, and `ON-RETURN` is consulted. When `HANDLER` is
`NIL`, no addition action is performed when a matching condition is
found.

The default `CTX` describes the result of the matching process in
terms of [`*CONDITION-MATCHED-P*`][1d8e] and [`*BEST-MATCHING-CONDITION*`][9f9b].

<a id='x-28TRY-3A-2ACONDITION-MATCHED-P-2A-20VARIABLE-29'></a>

- [variable] **\*CONDITION-MATCHED-P\*** *"-unbound-"*

    When a check described in [Checking Conditions][1955] signals its
    [`OUTCOME`][a306], this variable is bound to a boolean value to indicate
    whether a condition that matched `CONDITION-TYPE` and `PRED` was
    found.

<a id='x-28TRY-3A-2ABEST-MATCHING-CONDITION-2A-20VARIABLE-29'></a>

- [variable] **\*BEST-MATCHING-CONDITION\*** *"-unbound-"*

    Bound when a check described in [Checking Conditions][1955]
    signals its [`OUTCOME`][a306]. If [`*CONDITION-MATCHED-P*`][1d8e], then it is the
    most recent condition that matched both `CONDITION-TYPE` and `PRED`.
    Else, it is the most recent condition that matched
    `CONDITION-TYPE` or `NIL` if no such conditions were detected.

<a id='x-28TRY-3ASIGNALS-20MGL-PAX-3AMACRO-29'></a>

- [macro] **SIGNALS** *(CONDITION-TYPE &KEY PRED (HANDLER T) (ON-RETURN T) (ON-NLX T) NAME MSG CTX) &BODY BODY*

    Check that `BODY` signals a `CONDITION` of `CONDITION-TYPE` (not
    evaluated) that matches `PRED`. To detect matching conditions, `SIGNALS`
    sets up a `HANDLER-BIND`. Thus it can only see what `BODY` does not
    handle. The arguments are described in [Checking Conditions][1955].
    
    ```
    (signals (error)
      (error "xxx"))
    => NIL
    ```
    
    The following example shows a failure where `CONDITION-TYPE` matches
    but `PRED` does not.
    
    ```
    (ignore-errors
      (signals (error :pred "non-matching")
        (error "xxx")))
    
    UNEXPECTED-FAILURE in check:
      (ERROR "xxx") signals a condition of type ERROR that matches
      "non-matching".
    The predicate did not match "xxx".
    ```


<a id='x-28TRY-3ASIGNALS-NOT-20MGL-PAX-3AMACRO-29'></a>

- [macro] **SIGNALS-NOT** *(CONDITION-TYPE &KEY PRED (HANDLER T) (ON-RETURN T) (ON-NLX T) NAME MSG CTX) &BODY BODY*

    Check that `BODY` does not signal a `CONDITION` of `CONDITION-TYPE` (not
    evaluated) that matches `PRED`. To detect matching conditions,
    `SIGNALS-NOT` sets up a `HANDLER-BIND`. Thus it can only see what `BODY`
    does not handle. The arguments are described in
    [Checking Conditions][1955].

<a id='x-28TRY-3AINVOKES-DEBUGGER-20MGL-PAX-3AMACRO-29'></a>

- [macro] **INVOKES-DEBUGGER** *(CONDITION-TYPE &KEY PRED (HANDLER T) (ON-RETURN T) (ON-NLX T) NAME MSG CTX) &BODY BODY*

    Check that `BODY` enters the debugger with a `CONDITION` of
    `CONDITION-TYPE` (not evaluated) that matches `PRED`. To detect matching
    conditions, `INVOKES-DEBUGGER` sets up a `*DEBUGGER-HOOK*`. Thus if
    `*DEBUGGER-HOOK*` is changed by `BODY`, it may not detect the condition.
    The arguments are described in [Checking Conditions][1955].
    
    Note that in a trial (see [`CURRENT-TRIAL`][e542]), all `ERROR`s are handled,
    and a `*DEBUGGER-HOOK*` is set up (see [`UNHANDLED-ERROR`][e65b]). Thus invoking
    debugger would normally cause the trial to abort.
    
    ```
    (invokes-debugger (error :pred "xxx")
      (handler-bind ((error #'invoke-debugger))
        (error "xxx")))
    => NIL
    ```


<a id='x-28TRY-3AINVOKES-DEBUGGER-NOT-20MGL-PAX-3AMACRO-29'></a>

- [macro] **INVOKES-DEBUGGER-NOT** *(CONDITION-TYPE &KEY PRED (HANDLER T) (ON-RETURN T) (ON-NLX T) NAME MSG CTX) &BODY BODY*

    Check that `BODY` does not enter the debugger with a `CONDITION` of
    `CONDITION-TYPE` (not evaluated) that matches `PRED`. To detect matching
    conditions, `INVOKES-DEBUGGER-NOT` sets up a `*DEBUGGER-HOOK*`. Thus if
    `*DEBUGGER-HOOK*` is changed by `BODY`, it may not detect the condition.
    The arguments are described in [Checking Conditions][1955].

<a id='x-28TRY-3A-40TRY-2FMISC-CHECKS-20MGL-PAX-3ASECTION-29'></a>

### 6.2 Miscellaneous Checks

<a id='x-28TRY-3AFAILS-20MGL-PAX-3AMACRO-29'></a>

- [macro] **FAILS** *(&KEY NAME MSG CTX) &BODY BODY*

    Check that `BODY` performs a [non-local exit][c4d2] but do not cancel
    it (see [cancelled non-local exit][d65b]). See [Check Library][4fbb] for the descriptions
    of the other arguments.
    
    In the following example, `FAILS` signals a [`SUCCESS`][440d].
    
    ```
    (catch 'foo
      (fails ()
        (throw 'foo 7)))
    => 7
    ```
    
    Next, `FAILS` signals an [`UNEXPECTED-FAILURE`][10b2] because `BODY` returns
    normally.
    
    ```
    (fails ()
      (print 'hey))
    
    UNEXPECTED-FAILURE in check:
      (PRINT 'HEY) does not return normally.
    ```
    
    Note that there is no `FAILS-NOT` as [`WITH-TEST`][af8d] fills that role.

<a id='x-28TRY-3AIN-TIME-20MGL-PAX-3AMACRO-29'></a>

- [macro] **IN-TIME** *(SECONDS &KEY (ON-RETURN T) (ON-NLX T) NAME MSG CTX) &BODY BODY*

    Check that `BODY` finishes in `SECONDS`. See [Check Library][4fbb] for
    the descriptions of the other arguments.
    
    ```
    (in-time (1)
      (sleep 2))
    
    UNEXPECTED-FAILURE in check:
      (SLEEP 2) finishes within 1s.
    Took 2.000s.
    ```
    
    [`RETRY-CHECK`][0d2f] restarts timing.

<a id='x-28TRY-3A-2AIN-TIME-ELAPSED-SECONDS-2A-20VARIABLE-29'></a>

- [variable] **\*IN-TIME-ELAPSED-SECONDS\*** *"-unbound-"*

    Bound to the number of seconds passed during the evaluation of
    `BODY` when [`IN-TIME`][6249] signals its [`OUTCOME`][a306].

<a id='x-28TRY-3A-40TRY-2FCHECK-UTILITIES-20MGL-PAX-3ASECTION-29'></a>

### 6.3 Check Utilities

These utilities are not checks (which signal [`OUTCOME`][a306]s) but simple
functions and macros that may be useful for writing [`IS`][ff02] checks.

<a id='x-28TRY-3AON-VALUES-20MGL-PAX-3AMACRO-29'></a>

- [macro] **ON-VALUES** *FORM &BODY BODY*

    `ON-VALUES` evaluates `FORM` and transforms its return values one by
    one based on forms in `BODY`. The Nth value is replaced by the return
    value of the Nth form of `BODY` evaluated with `*` bound to the Nth
    value. If the number of values exceeds the number of transformation
    forms in `BODY` then the excess values are returned as is.
    
    ```
    (on-values (values 1 "abc" 7)
      (1+ *)
      (length *))
    => 2
    => 3
    => 7
    ```
    
    If the number of values is less than the number of transformation
    forms, then in later transformation forms `*` is bound to `NIL`.
    
    ```
    (on-values (values)
      *
      *)
    => NIL
    => NIL
    ```
    
    The first forms in `BODY` may be options. Options must precede
    transformation forms. With `:TRUNCATE` `T`, the excess values are
    discarded.
    
    ```
    (on-values (values 1 "abc" 7)
      (:truncate t)
      (1+ *)
      (length *))
    => 2
    => 3
    ```
    
    The `:ON-LENGTH-MISMATCH` option may be `NIL` or a function of a single
    argument. If the number of values and the number of transformation
    forms is different, then this function is called to transform the
    list of values. `:TRUNCATE` is handled before `:ON-LENGTH-MISMATCH`.
    
    ```
    (on-values 1
      (:on-length-mismatch (lambda (values)
                             (if (= (length values) 1)
                                 (append values '("abc"))
                                 values)))
      (1+ *)
      *)
    => 2
    => "abc"
    ```
    
    If the same option is specified multiple times, only the first one
    is in effect.

<a id='x-28TRY-3AMATCH-VALUES-20MGL-PAX-3AMACRO-29'></a>

- [macro] **MATCH-VALUES** *FORM &BODY BODY*

    `MATCH-VALUES` returns true iff all return values of `FORM` satisfy
    the predicates given by `BODY`, which are described in [`ON-VALUES`][87f6]. The
    `:TRUNCATE` option of `ON-VALUES` is supported, but `:ON-LENGTH-MISMATCH`
    always returns `NIL`.
    
    ```
    ;; no values
    (is (match-values (values)))
    ;; single value success
    (is (match-values 1
          (= * 1)))
    ;; success with different types
    (is (match-values (values 1 "sdf")
          (= * 1)
          (string= * "sdf")))
    ;; too few values
    (is (not (match-values 1
               (= * 1)
               (string= * "sdf"))))
    ;; too many values
    (is (not (match-values (values 1 "sdf" 3)
               (= * 1)
               (string= * "sdf"))))
    ;; too many values, but truncated
    (is (match-values (values 1 "sdf" 3)
          (:truncate t)
          (= * 1)
          (string= * "sdf")))
    ```


<a id='x-28TRY-3AMISMATCH-25-20FUNCTION-29'></a>

- [function] **MISMATCH%** *SEQUENCE1 SEQUENCE2 &KEY FROM-END (TEST #'EQL) (START1 0) END1 (START2 0) END2 KEY MAX-PREFIX-LENGTH MAX-SUFFIX-LENGTH*

    Like `CL:MISMATCH` but `CAPTUREs` and returns the common prefix and
    the mismatched suffixes. The `TEST-NOT` argument is deprecated by
    the CLHS and is not supported. In addition, if `MAX-PREFIX-LENGTH` and
    `MAX-SUFFIX-LENGTH` are non-`NIL`, they must be non-negative integers,
    and they limit the number of elements in the prefix and the
    suffixes.
    
    ```
    (is (null (mismatch% '(1 2 3) '(1 2 4 5))))
    
    UNEXPECTED-FAILURE in check:
      (IS (NULL #1=(MISMATCH% '(1 2 3) '(1 2 4 5))))
    where
      COMMON-PREFIX = (1 2)
      MISMATCHED-SUFFIX-1 = (3)
      MISMATCHED-SUFFIX-2 = (4 5)
      #1# = 2
    ```
    
    ```
    (is (null (mismatch% "Hello, World!"
                          "Hello,
    World!")))
    
    UNEXPECTED-FAILURE in check:
      (IS
       (NULL
        #1=(MISMATCH% "Hello, World!" "Hello,
      World!")))
    where
      COMMON-PREFIX = "Hello,"
      MISMATCHED-SUFFIX-1 = " World!"
      MISMATCHED-SUFFIX-2 = "
      World!"
      #1# = 6
    ```


<a id='x-28TRY-3ADIFFERENT-ELEMENTS-20FUNCTION-29'></a>

- [function] **DIFFERENT-ELEMENTS** *SEQUENCE1 SEQUENCE2 &KEY (PRED #'EQL) (MISSING :MISSING)*

    Return the different elements under `PRED` in the given sequences as
    a list of `(:INDEX <INDEX> <E1> <E2>)` elements, where `E1` and `E2`
    are elements of `SEQUENCE1` and `SEQUENCE2` at `<INDEX>`, respectively,
    and they may be `MISSING` if the corresponding sequence is too short.
    
    ```
    (is (endp (different-elements '(1 2 3) '(1 b 3 d))))
    
    UNEXPECTED-FAILURE in check:
      (IS (ENDP #1=(DIFFERENT-ELEMENTS '(1 2 3) '(1 B 3 D))))
    where
      #1# = ((:INDEX 1 2 B) (:INDEX 3 :MISSING D))
    ```


<a id='x-28TRY-3AWITH-SHUFFLING-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-SHUFFLING** *NIL &BODY BODY*

    Execute the forms that make up the list of forms `BODY` in random
    order and return `NIL`. This may be useful to prevent writing tests
    that accidentally depend on the order in which subtests are called.
    
    ```
    (loop repeat 3 do
      (with-shuffling ()
        (prin1 1)
        (prin1 2)))
    .. 122112
    => NIL
    ```


<a id='x-28TRY-3A-40TRY-2FCOMPARING-FLOATS-20MGL-PAX-3ASECTION-29'></a>

#### 6.3.1 Comparing Floats

Float comparisons following
[https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/](https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/).

<a id='x-28TRY-3AFLOAT--7E-3D-20FUNCTION-29'></a>

- [function] **FLOAT-~=** *X Y &KEY (MAX-DIFF-IN-VALUE \*MAX-DIFF-IN-VALUE\*) (MAX-DIFF-IN-ULP \*MAX-DIFF-IN-ULP\*)*

    Return whether two numbers, `X` and `Y`, are approximately equal either
    according to `MAX-DIFF-IN-VALUE` or `MAX-DIFF-IN-ULP`.
    
    If the absolute value of the difference of two floats is not greater
    than `MAX-DIFF-IN-VALUE`, then they are considered equal.
    
    If two floats are of the same sign and the number of representable
    floats (ULP, unit in the last place) between them is less than
    `MAX-DIFF-IN-ULP`, then they are considered equal.
    
    If neither `X` nor `Y` are floats, then the comparison is done with `=`.
    If one of them is a `DOUBLE-FLOAT`, then the other is converted to a
    double float, and the comparison takes place in double float space.
    Else, both are converted to `SINGLE-FLOAT` and the comparison takes
    place in single float space.

<a id='x-28TRY-3A-2AMAX-DIFF-IN-VALUE-2A-20VARIABLE-29'></a>

- [variable] **\*MAX-DIFF-IN-VALUE\*** *1.0e-16*

    The default value of the `MAX-DIFF-IN-VALUE` argument of [`FLOAT-~=`][7397].

<a id='x-28TRY-3A-2AMAX-DIFF-IN-ULP-2A-20VARIABLE-29'></a>

- [variable] **\*MAX-DIFF-IN-ULP\*** *2*

    The default value of the `MAX-DIFF-IN-ULP` argument of [`FLOAT-~=`][7397].

<a id='x-28TRY-3AFLOAT--7E-3C-20FUNCTION-29'></a>

- [function] **FLOAT-~\<** *X Y &KEY (MAX-DIFF-IN-VALUE \*MAX-DIFF-IN-VALUE\*) (MAX-DIFF-IN-ULP \*MAX-DIFF-IN-ULP\*)*

    Return whether `X` is approximately less than `Y`. Equivalent to `<`,
    but it also allows for approximate equality according to [`FLOAT-~=`][7397].

<a id='x-28TRY-3AFLOAT--7E-3E-20FUNCTION-29'></a>

- [function] **FLOAT-~\>** *X Y &KEY (MAX-DIFF-IN-VALUE \*MAX-DIFF-IN-VALUE\*) (MAX-DIFF-IN-ULP \*MAX-DIFF-IN-ULP\*)*

    Return whether `X` is approximately greater than `Y`. Equivalent to `>`,
    but it also allows for approximate equality according to [`FLOAT-~=`][7397].

<a id='x-28TRY-3A-40TRY-2FTESTS-20MGL-PAX-3ASECTION-29'></a>

## 7 Tests

In Try, tests are Lisp functions that record their execution in
[`TRIAL`][9fc3] objects. `TRIAL`s are to tests what function call traces are to
functions. In more detail, tests

- create a `TRIAL` object and signal a [`TRIAL-START`][a231] event upon entry to
  the function,

- signal a [`VERDICT`][5976] condition before returning normally or via a
  [non-local exit][c4d2],

- return the `TRIAL` object as the first value,

- return explicitly returned values as the second, third, and so on
  values.

See [`DEFTEST`][e6a7] and [`WITH-TEST`][af8d] for more precise descriptions.

<a id='x-28TRY-3ADEFTEST-20MGL-PAX-3AMACRO-29'></a>

- [macro] **DEFTEST** *NAME LAMBDA-LIST &BODY BODY*

    `DEFTEST` is a wrapper around `DEFUN` to define global test functions.
    See `DEFUN` for a description of `NAME`, `LAMBDA-LIST`, and `BODY`. The
    behaviour common with [`WITH-TEST`][af8d] is described in [Tests][51bb].
    
    ```
    (deftest my-test ()
      (write-string "hey"))
    => MY-TEST
    
    (test-bound-p 'my-test)
    => T
    
    (my-test)
    .. hey
    ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s>
    ```
    
    Although the common case is for tests to have no arguments, `DEFTEST`
    supports general function lambda lists. Within a global test,
    
    - `NAME` is bound to the [`TRIAL`][9fc3] object
    
    - the first return value is the trial
    
    - values are not returned implicitly
    
    - values returned with an explicit `RETURN-FROM` are returned as
      values after the trial
    
    ```
    (deftest my-test ()
      (prin1 my-test)
      (return-from my-test (values 2 3)))
    
    (my-test)
    .. #<TRIAL (MY-TEST) RUNNING>
    ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s>
    => 2
    => 3
    ```


<a id='x-28TRY-3A-2ARUN-DEFTEST-WHEN-2A-20VARIABLE-29'></a>

- [variable] **\*RUN-DEFTEST-WHEN\*** *NIL*

    This may be any of `:COMPILE-TOPLEVEL`, `:LOAD-TOPLEVEL`, `:EXECUTE`, or
    a list thereof. The value of `*RUN-DEFTEST-WHEN*` determines in what
    `EVAL-WHEN` situation to call the test function immediately after it
    has been defined with [`DEFTEST`][e6a7].
    
    For interactive development, it may be convenient to set it to
    `:EXECUTE` and have the test run when the `DEFTEST` is evaluated (maybe
    with Slime `C-M-x`, `slime-eval-defun`). Or set it to
    `:COMPILE-TOPLEVEL`, and have it rerun on Slime `C-c C-c`,
    `slime-compile-defun`.
    
    If the test has required arguments, an argument list is prompted for
    and read from `*QUERY-IO*`.

<a id='x-28TRY-3ATEST-BOUND-P-20FUNCTION-29'></a>

- [function] **TEST-BOUND-P** *SYMBOL*

    See if `SYMBOL` names a global test (i.e. a test defined with
    [`DEFTEST`][e6a7]). If since the execution of `DEFTEST`, the symbol has been
    uninterned, `FMAKUNBOUND`, or redefined with `DEFUN`, then it no longer
    names a global test.

<a id='x-28TRY-3AWITH-TEST-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-TEST** *(&OPTIONAL TRIAL-VAR &KEY NAME) &BODY BODY*

    Define a so-called lambda test to group together `CHECK`s and other
    tests it executes. `WITH-TEST` executes `BODY` in its lexical
    environment even on a rerun (see [Rerunning Trials][76af]).
    
    If `TRIAL-VAR` is a non-`NIL` symbol, bind it to the trial object.
    `NAME` may be any type, it is purely for presentation purposes. If
    `NAME` is `NIL`, then it defaults to `TRIAL-VAR`.
    
    To facilitate returning values, a `BLOCK` is wrapped around `BODY`. The
    name of the block is `TRIAL-VAR` if it is a symbol, else it's `NIL`.
    
    When both `TRIAL-VAR` and `NAME` are specified:
    
    ```
    (with-test (some-feature :name "obscure feature")
      (prin1 some-feature)
      (is t)
      (return-from some-feature (values 1 2)))
    .. #<TRIAL (WITH-TEST ("obscure feature")) RUNNING>
    .. obscure feature
    ..   ⋅ (IS T)
    .. ⋅ obscure feature ⋅1
    ..
    ==> #<TRIAL (WITH-TEST ("obscure feature")) EXPECTED-SUCCESS 0.000s ⋅1>
    => 1
    => 2
    ```
    
    If only `TRIAL-VAR` is specified:
    
    ```
    (with-test (some-feature)
      (prin1 some-feature)
      (is t)
      (return-from some-feature (values 1 2)))
    .. #<TRIAL (WITH-TEST (SOME-FEATURE)) RUNNING>
    .. SOME-FEATURE
    ..   ⋅ (IS T)
    .. ⋅ SOME-FEATURE ⋅1
    ..
    ==> #<TRIAL (WITH-TEST (SOME-FEATURE)) EXPECTED-SUCCESS 0.000s ⋅1>
    => 1
    => 2
    ```
    
    If neither is specified:
    
    ```
    (with-test ()
      (prin1 (current-trial))
      (is t)
      (return (values 1 2)))
    .. #<TRIAL (WITH-TEST (NIL)) RUNNING>
    .. NIL
    ..   ⋅ (IS T)
    .. ⋅ NIL ⋅1
    ..
    ==> #<TRIAL (WITH-TEST (NIL)) EXPECTED-SUCCESS 0.000s ⋅1>
    => 1
    => 2
    ```
    
    Finally, using that `NAME` defaults to `TRIAL-VAR` and that it is valid
    to specify non-symbols for `TRIAL-VAR`, one can also write:
    
    ```
    (with-test ("Some feature")
      (prin1 (current-trial))
      (is t)
      (return (values 1 2)))
    .. #<TRIAL (WITH-TEST ("Some feature")) RUNNING>
    .. Some feature
    ..   ⋅ (IS T)
    .. ⋅ Some feature ⋅1
    ..
    ==> #<TRIAL (WITH-TEST ("Some feature")) EXPECTED-SUCCESS 0.000s ⋅1>
    => 1
    => 2
    ```
    
    In summary and in contrast to global tests (those defined with
    [`DEFTEST`][e6a7]), lambda tests
    
    - have no arguments,
    
    - are defined and called at the same time,
    
    - may not bind their trial object to any variable,
    
    - may have a `BLOCK` named `NIL`,
    
    - have a `NAME` purely for presentation purposes.
    
    Lambda tests can be thought of as analogous to `(FUNCALL (LAMBDA ()
    BODY))`. The presence of the `LAMBDA` is important because it is
    stored in the [`TRIAL`][9fc3] object to support [Rerunning Trials][76af].

<a id='x-28TRY-3ALIST-PACKAGE-TESTS-20FUNCTION-29'></a>

- [function] **LIST-PACKAGE-TESTS** *&OPTIONAL (PACKAGE \*PACKAGE\*)*

    List all symbols in `PACKAGE` that name global tests in the sense of
    [`TEST-BOUND-P`][e55e].

<a id='x-28TRY-3AWITH-TESTS-RUN-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-TESTS-RUN** *(TESTS-RUN) &BODY BODY*

    Bind the symbol `TESTS-RUN` to an empty `EQ` hash table and execute
    `BODY`. The has table reflects call counts to global tests. Keys are
    symbols naming global tests, and the values are the number of times
    the keys have been called.

<a id='x-28TRY-3AWARN-ON-TESTS-NOT-RUN-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WARN-ON-TESTS-NOT-RUN** *(&OPTIONAL (PACKAGE \*PACKAGE\*)) &BODY BODY*

    A convenience utility to that records the global tests run by `BODY`
    with [`WITH-TESTS-RUN`][8389] and, when `BODY` finishes, signals a warning for
    each global tests in `PACKAGE` not run.
    
    This is how Try runs its own tests:
    
    ```
    (defun test ()
      ;; Bind *PACKAGE* so that names of tests printed have package names,
      ;; and M-. works on them in Slime.
      (let ((*package* (find-package :common-lisp)))
        (warn-on-tests-not-run ((find-package :try))
          (print (try 'test-all
                      :print 'unexpected
                      :describe 'unexpected)))))
    ```


<a id='x-28TRY-3A-40TRY-2FIMPLICIT-TRY-20MGL-PAX-3ASECTION-29'></a>

### 7.1 Calling Test Functions

Tests can be run explicitly by invoking the [`TRY`][7a62] function or
implicitly by calling a test function:

```
(deftest my-test ()
  (is t))

(my-test)
.. MY-TEST
..   ⋅ (IS T)
.. ⋅ MY-TEST ⋅1
..
==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.004s ⋅1>
```

The situation is similar with a [`WITH-TEST`][af8d]:

```
(with-test (my-test)
  (is t))
.. MY-TEST
..   ⋅ (IS T)
.. ⋅ MY-TEST ⋅1
..
==> #<TRIAL (WITH-TEST (MY-TEST)) EXPECTED-SUCCESS 0.000s ⋅1>
```

Behind the scenes, the outermost test function calls `TRY` with

```
(try trial :debug *debug* :collect *collect* :rerun *rerun*
     :print *print* :describe *describe*
     :stream *stream* :printer *printer*)
```

`TRY` then calls the test function belonging to `TRIAL`.
The rest of the behaviour is described in [Explicit TRY][6c25].

<a id='x-28TRY-3A-2ADEBUG-2A-20VARIABLE-29'></a>

- [variable] **\*DEBUG\*** *(AND UNEXPECTED (NOT NLX) (NOT VERDICT))*

    The default value makes [`TRY`][7a62] invoke the debugger on [`UNHANDLED-ERROR`][e65b],
    [`RESULT-ABORT*`][4c47], [`UNEXPECTED-RESULT-FAILURE`][8ce0], and
    [`UNEXPECTED-RESULT-SUCCESS`][87a0]. [`NLX`][d43d] is excluded because it is caught as
    the test function is being exited, but by that time the dynamic
    environment of the actual cause is likely gone. [`VERDICT`][5976] is excluded
    because it is a consequence of its child outcomes.

<a id='x-28TRY-3A-2ACOUNT-2A-20VARIABLE-29'></a>

- [variable] **\*COUNT\*** *LEAF*

    Although the default value of [`*CATEGORIES*`][3d4c] lumps [`RESULT`][3091]s and
    [`VERDICT`][5976]s together, with the default of [`LEAF`][1ec2], `VERDICT`s are not
    counted.

<a id='x-28TRY-3A-2ACOLLECT-2A-20VARIABLE-29'></a>

- [variable] **\*COLLECT\*** *UNEXPECTED*

    To save memory, only the [`UNEXPECTED`][077b] are collected by default.

<a id='x-28TRY-3A-2ARERUN-2A-20VARIABLE-29'></a>

- [variable] **\*RERUN\*** *UNEXPECTED*

    The default matches that of [`*COLLECT*`][1dfa].

<a id='x-28TRY-3A-2APRINT-2A-20VARIABLE-29'></a>

- [variable] **\*PRINT\*** *LEAF*

    With the default of [`LEAF`][1ec2] combined with the default [`*PRINT-PARENT*`][85d7]
    `T`, only [`TRIAL`][9fc3]s with checks or [`ERROR*`][e6dd] in them are printed. If
    [`UNEXPECTED`][077b], only the interesting things are printed.

<a id='x-28TRY-3A-2ADESCRIBE-2A-20VARIABLE-29'></a>

- [variable] **\*DESCRIBE\*** *UNEXPECTED*

    By default, the context (e.g. [Captures][a03c], and the `CTX` argument
    of is and other checks) of [`UNEXPECTED`][077b] events is described.

<a id='x-28TRY-3A-2ASTREAM-2A-20-28VARIABLE-20-28MAKE-SYNONYM-STREAM-20-28QUOTE-20-2ADEBUG-IO-2A-29-29-29-29'></a>

- [variable] **\*STREAM\*** *(MAKE-SYNONYM-STREAM '\*DEBUG-IO\*)*

<a id='x-28TRY-3A-2APRINTER-2A-20VARIABLE-29'></a>

- [variable] **\*PRINTER\*** *TREE-PRINTER*

<a id='x-28TRY-3A-40TRY-2FEXPLICIT-TRY-20MGL-PAX-3ASECTION-29'></a>

### 7.2 Explicit TRY

Instead of invoking the test function directly, tests can also be
run by invoking the [`TRY`][7a62] function.

```
(deftest my-test ()
  (is t))

(try 'my-test)
.. MY-TEST
..   ⋅ (IS T)
.. ⋅ MY-TEST ⋅1
..
==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s ⋅1>
```

The situation is similar with a [`WITH-TEST`][af8d], only that `TRY` wraps an
extra [`TRIAL`][9fc3] around the execution of the `LAMBDA` to ensure that all
[`EVENT`][6ded]s are signalled within a trial.

```
(try (lambda ()
       (with-test (my-test)
         (is t))))
.. (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>)
..   MY-TEST
..     ⋅ (IS T)
..   ⋅ MY-TEST ⋅1
.. ⋅ (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>) ⋅1
..
==> #<TRIAL (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>) EXPECTED-SUCCESS 0.000s ⋅1>
```

Invoking tests with an explicit `TRY` is very similar to just calling
the test functions directly (see [Calling Test Functions][0fb2]). The differences
are that `TRY`

- can run [Testables][d270],

- has a function argument for each of the [`*DEBUG*`][0482], [`*COLLECT*`][1dfa], etc
  variables.

Those arguments default to [`*TRY-DEBUG*`][70d8], [`*TRY-COLLECT*`][9379], etc, which
parallel and default to `*DEBUG*`, `*COLLECT*`, etc if set to
`:UNSPECIFIED`. `*TRY-DEBUG*` is `NIL`, the rest of them are `:UNSPECIFIED`.

These defaults encourage the use of an explicit `TRY` call in the
non-interactive case and calling the test functions directly in the
interactive one, but this is not enforced in any way.

<a id='x-28TRY-3ATRY-20FUNCTION-29'></a>

- [function] **TRY** *TESTABLE &KEY (DEBUG \*TRY-DEBUG\*) (COUNT \*TRY-COUNT\*) (COLLECT \*TRY-COLLECT\*) (RERUN \*TRY-RERUN\*) (PRINT \*TRY-PRINT\*) (DESCRIBE \*TRY-DESCRIBE\*) (STREAM \*TRY-STREAM\*) (PRINTER \*TRY-PRINTER\*)*

    `TRY` runs `TESTABLE` and handles the [`EVENT`][6ded]s to collect, debug, print
    the results of checks and trials, and to decide what tests to skip
    and what to rerun.
    
    `DEBUG`, `COUNT`, `COLLECT`, `RERUN`, `PRINT`, and `DESCRIBE` must all be valid
    specifiers for types that are either `NIL` (the empty type) or have a
    non-empty intersection with the type `EVENT` (e.g. `T`, [`OUTCOME`][a306],
    [`UNEXPECTED`][077b], [`VERDICT`][5976]).
    
    `TRY` sets up a `HANDLER-BIND` handler for `EVENT`s and runs `TESTABLE` (see
    [Testables][d270]). When an `EVENT` is signalled, the handler matches its
    type to the value of the `DEBUG` argument (in the sense of `(TYPEP
    EVENT DEBUG)`). If it matches, then the debugger is invoked with the
    event. In the debugger, the user has a number of restarts available
    to change (see [Event Restarts][e852], [Outcome Restarts][14a6],
    [Check Restarts][dbd2], [Trial Restarts][dbf0], and [`SET-TRY-DEBUG`][068f].
    
    If the debugger is not invoked, `TRY` invokes the very first restart
    available, which is always [`RECORD-EVENT`][318d].
    
    Recording the event is performed as follows.
    
    - Outcome counts are updated (see [Counting Events][3c27]).
    
    - The event is passed to the collector (see [Collecting Events][0a38]).
    
    - The event is passed to the printer (see [Printing Events][53fc]).
    
    - Finally, when rerunning a trial (i.e. when `TESTABLE` is a trial),
      on a [`TRIAL-START`][a231] event, the trial may be skipped (see [Rerunning Trials][76af]).
    
    `TRY` returns the values returned by the outermost trial (see
    [Tests][51bb]).

<a id='x-28TRY-3ASET-TRY-DEBUG-20FUNCTION-29'></a>

- [function] **SET-TRY-DEBUG** *DEBUG*

    Invoke the `SET-TRY-DEBUG` restart to override the `DEBUG` argument of
    the currently running [`TRY`][7a62]. `DEBUG` must thus be a suitable type. When
    the `SET-TRY-DEBUG` restart is invoked interactively, `DEBUG` is read as
    a non-evaluated form from `*QUERY-IO*`.

<a id='x-28TRY-3A-2ATRY-DEBUG-2A-20VARIABLE-29'></a>

- [variable] **\*TRY-DEBUG\*** *NIL*

    The default value for [`TRY`][7a62]'s `:DEBUG` argument. If
    `:UNSPECIFIED`, then the value of [`*DEBUG*`][0482] is used instead.

<a id='x-28TRY-3A-2ATRY-COUNT-2A-20VARIABLE-29'></a>

- [variable] **\*TRY-COUNT\*** *:UNSPECIFIED*

    The default value for [`TRY`][7a62]'s `:COUNT` argument. If
    `:UNSPECIFIED`, then the value of [`*COUNT*`][7eb2] is used instead.

<a id='x-28TRY-3A-2ATRY-COLLECT-2A-20VARIABLE-29'></a>

- [variable] **\*TRY-COLLECT\*** *:UNSPECIFIED*

    The default value for [`TRY`][7a62]'s `:COLLECT` argument. If
    `:UNSPECIFIED`, then the value of [`*COLLECT*`][1dfa] is used instead.

<a id='x-28TRY-3A-2ATRY-RERUN-2A-20VARIABLE-29'></a>

- [variable] **\*TRY-RERUN\*** *:UNSPECIFIED*

    The default value for [`TRY`][7a62]'s `:RERUN` argument. If
    `:UNSPECIFIED`, then the value of [`*RERUN*`][0e2a] is used instead.

<a id='x-28TRY-3A-2ATRY-PRINT-2A-20VARIABLE-29'></a>

- [variable] **\*TRY-PRINT\*** *:UNSPECIFIED*

    The default value for [`TRY`][7a62]'s `:PRINT` argument. If
    `:UNSPECIFIED`, then the value of [`*PRINT*`][406c] is used instead.

<a id='x-28TRY-3A-2ATRY-DESCRIBE-2A-20VARIABLE-29'></a>

- [variable] **\*TRY-DESCRIBE\*** *:UNSPECIFIED*

    The default value for [`TRY`][7a62]'s `:DESCRIBE` argument. If
    `:UNSPECIFIED`, then the value of [`*DESCRIBE*`][95df] is used instead.

<a id='x-28TRY-3A-2ATRY-STREAM-2A-20VARIABLE-29'></a>

- [variable] **\*TRY-STREAM\*** *:UNSPECIFIED*

    The default value for [`TRY`][7a62]'s `:STREAM` argument. If
    `:UNSPECIFIED`, then the value of [`*STREAM*`][888d] is used instead.

<a id='x-28TRY-3A-2ATRY-PRINTER-2A-20VARIABLE-29'></a>

- [variable] **\*TRY-PRINTER\*** *:UNSPECIFIED*

    The default value for [`TRY`][7a62]'s `:PRINTER` argument. If
    `:UNSPECIFIED`, then the value of [`*PRINTER*`][6866] is used instead.

<a id='x-28TRY-3A-2AN-RECENT-TRIALS-2A-20VARIABLE-29'></a>

- [variable] **\*N-RECENT-TRIALS\*** *3*

    See `*RECENT-TRIALS*`.

<a id='x-28TRY-3ARECENT-TRIAL-20FUNCTION-29'></a>

- [function] **RECENT-TRIAL** *&OPTIONAL (N 0)*

    Returns the `N`th most recent trial or `NIL` if there are not enough
    trials recorded. Every [`TRIAL`][9fc3] returned by [`TRY`][7a62] gets pushed
    onto a list of trials, but only [`*N-RECENT-TRIALS*`][10a6] are kept.

<a id='x-28TRY-3A-21-20-28VARIABLE-20NIL-29-29'></a>

- [variable] **!** *NIL*

    The most recent trial. Equivalent to `(RECENT-TRIAL 0)`.

<a id='x-28TRY-3A-21-21-20-28VARIABLE-20NIL-29-29'></a>

- [variable] **!!** *NIL*

    Equivalent to `(RECENT-TRIAL 1)`.

<a id='x-28TRY-3A-21-21-21-20-28VARIABLE-20NIL-29-29'></a>

- [variable] **!!!** *NIL*

    Equivalent to `(RECENT-TRIAL 2)`.

<a id='x-28TRY-3A-40TRY-2FTESTABLES-20MGL-PAX-3ASECTION-29'></a>

#### 7.2.1 Testables

Valid first arguments to [`TRY`][7a62] are called testables. A testable may
be:

- a [function designator][efc2]

    - the name of a global test

    - the name of a global function

    - a function object

    - a trial

- a list of testables

- a `PACKAGE`

In the function designator cases, `TRY` calls the designated function.
[`TRIAL`][9fc3]s, being [funcallable instance][de86]s, designate themselves. If the
trial is not [`RUNNINGP`][c773], then it will be rerun (see [Rerunning Trials][76af]). Don't
invoke `TRY` with `RUNNINGP` trials (but see
[Implementation of Implicit TRY][303f] for discussion).

When given a list of testables, `TRY` calls each testable one by one.

Finally, a `PACKAGE` stands for the result of calling
[`LIST-PACKAGE-TESTS`][9e9e] on that package.

<a id='x-28TRY-3A-40TRY-2FIMPLICIT-TRY-IMPLEMENTATION-20MGL-PAX-3ASECTION-29'></a>

#### 7.2.2 Implementation of Implicit TRY

What's happening in the implementation is that a test function,
when it is called, checks whether it is running under the [`TRY`][7a62]
function. If it isn't, then it invokes `TRY` with its [`TRIAL`][9fc3]. `TRY`
realizes the trial cannot be rerun yet (see [Rerunning Trials][76af]) because it
is [`RUNNINGP`][c773], sets up its event handlers for debugging, collecting,
printing, and invokes the trial as if it were rerun but without
skipping anything based on the `RERUN` argument. Thus the following
are infinite recursions:

```
(with-test (recurse)
  (try recurse))

(with-test (recurse)
  (funcall recurse))
```


<a id='x-28TRY-3A-40TRY-2FPRINT-20MGL-PAX-3ASECTION-29'></a>

### 7.3 Printing Events

[`TRY`][7a62] instantiates a printer of the type given by its `PRINTER`
argument. All [`EVENT`][6ded]s recorded by `TRY` are sent to this printer. The
printer then prints events that match the type given by the `PRINT`
argument of `TRY`. Events that also match the `DESCRIBE` argument of `TRY`
are printed with context information (see [`IS`][ff02]) and backtraces (see
[`UNHANDLED-ERROR`][e65b]).

Although the printing is primarily customized with global special
variables, changing the value of those variables after the printer
object is instantiated by `TRY` has no effect. This is to ensure
consistent output with nested `TRY` calls of differing printer
setups.

<a id='x-28TRY-3ATREE-PRINTER-20CLASS-29'></a>

- [class] **TREE-PRINTER**

    `TREE-PRINTER` prints events in an indented
    tree-like structure, with each internal node corresponding to a
    [`TRIAL`][9fc3]. This is the default printer (according to [`*PRINTER*`][6866] and
    [`*TRY-PRINTER*`][c4ab]) and currently the only one.
    
    The following example prints all [Concrete Events][8a03].
    
    ```
    (let ((*debug* nil)
          (*print* '(not trial-start))
          (*describe* nil))
      (with-test (verdict-abort*)
        (with-test (expected-verdict-success))
        (with-expected-outcome ('failure)
          (with-test (unexpected-verdict-success)))
        (handler-bind (((and verdict success) #'force-expected-failure))
          (with-test (expected-verdict-failure)))
        (handler-bind (((and verdict success) #'force-unexpected-failure))
          (with-test (unexpected-verdict-failure)))
        (with-test (verdict-skip)
          (skip-trial))
        (is t :msg "EXPECTED-RESULT-SUCCESS")
        (with-failure-expected ('failure)
          (is t :msg "UNEXPECTED-RESULT-SUCCESS")
          (is nil :msg "EXPECTED-RESULT-FAILURE"))
        (is nil :msg "UNEXPECTED-RESULT-FAILURE")
        (with-skip ()
          (is nil :msg "RESULT-SKIP"))
        (handler-bind (((and result success) #'abort-check))
          (is t :msg "RESULT-ABORT*"))
        (catch 'foo
          (with-test (nlx-test)
            (throw 'foo nil)))
        (error "UNHANDLED-ERROR")))
    .. VERDICT-ABORT*                       ; TRIAL-START
    ..   ⋅ EXPECTED-VERDICT-SUCCESS
    ..   ⊡ UNEXPECTED-VERDICT-SUCCESS
    ..   × EXPECTED-VERDICT-FAILURE
    ..   ⊠ UNEXPECTED-VERDICT-FAILURE
    ..   - VERDICT-SKIP
    ..   ⋅ EXPECTED-RESULT-SUCCESS
    ..   ⊡ UNEXPECTED-RESULT-SUCCESS
    ..   × EXPECTED-RESULT-FAILURE
    ..   ⊠ UNEXPECTED-RESULT-FAILURE
    ..   - RESULT-SKIP
    ..   ⊟ RESULT-ABORT*
    ..   NLX-TEST                           ; TRIAL-START
    ..     ⊟ non-local exit                 ; NLX
    ..   ⊟ NLX-TEST ⊟1                      ; VERDICT-ABORT*
    ..   ⊟ "UNHANDLED-ERROR" (SIMPLE-ERROR)
    .. ⊟ VERDICT-ABORT* ⊟3 ⊠1 -1 ×1 ⋅2
    ..
    ==> #<TRIAL (WITH-TEST (VERDICT-ABORT*)) ABORT* 0.004s ⊟3 ⊠1 ⊡1 -1 ×1 ⋅1>
    ```
    
    The `⊟3 ⊠1 ⊡1 -1 ×1 ⋅1` part is the counts for [`*CATEGORIES*`][3d4c] printed
    with their markers.

<a id='x-28TRY-3A-2APRINT-PARENT-2A-20VARIABLE-29'></a>

- [variable] **\*PRINT-PARENT\*** *T*

    When an [`EVENT`][6ded] is signalled and its parent [`TRIAL`][9fc3]'s type matches
    `*PRINT-PARENT*`, the trial is printed as if its [`TRIAL-START`][a231] matched
    the `PRINT` argument of [`TRY`][7a62].
    
    ```
    (let ((*print* 'leaf)
          (*print-parent* t))
      (with-test (t0)
        (is t)
        (is t)))
    .. T0
    ..   ⋅ (IS T)
    ..   ⋅ (IS T)
    .. ⋅ T0 ⋅2
    ..
    ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s ⋅2>
    ```
    
    ```
    (let ((*print* 'leaf)
          (*print-parent* nil))
      (with-test (t0)
        (is t)
        (is t)))
    .. ⋅ (IS T)
    .. ⋅ (IS T)
    ..
    ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s ⋅2>
    ```
    
    `*PRINT-PARENT*` `NIL` combined with printing [`VERDICT`][5976]s results in a flat
     output:
    
    ```
    (let ((*print* '(or leaf verdict))
          (*print-parent* nil))
      (with-test (outer)
        (with-test (inner)
          (is t :msg "inner-t"))
        (is t :msg "outer-t")))
    .. ⋅ inner-t
    .. ⋅ INNER ⋅1
    .. ⋅ outer-t
    .. ⋅ OUTER ⋅2
    ..
    ==> #<TRIAL (WITH-TEST (OUTER)) EXPECTED-SUCCESS 0.000s ⋅2>
    ```


<a id='x-28TRY-3A-2APRINT-INDENTATION-2A-20VARIABLE-29'></a>

- [variable] **\*PRINT-INDENTATION\*** *2*

    The number of spaces each printed [`TRIAL`][9fc3] increases the indentation
    of its children.

<a id='x-28TRY-3A-2APRINT-DURATION-2A-20VARIABLE-29'></a>

- [variable] **\*PRINT-DURATION\*** *NIL*

    If true, the number of seconds spent during execution is printed.
    
    ```
    (let ((*print-duration* t)
          (*debug* nil)
          (*describe* nil))
      (with-test (timed)
        (is (progn (sleep 0.3) t))
        (is (progn (sleep 0.2) t))
        (error "xxx")))
    ..        TIMED
    ..  0.300   ⋅ (IS (PROGN (SLEEP 0.3) T))
    ..  0.200   ⋅ (IS (PROGN (SLEEP 0.2) T))
    ..          ⊟ ""xxx (SIMPLE-ERROR)
    ..  0.504 ⊟ TIMED ⊟1 ⋅2
    ..
    ==> #<TRIAL (WITH-TEST (TIMED)) ABORT* 0.504s ⊟1 ⋅2>
    ```
    
    Timing is available for all [`OUTCOME`][a306]s (i.e. for [Checks][3e2d] and
    [`TRIAL`][9fc3]s). Checks generally measure the time spent during evaluation
    the form they are wrapping. Trials measure the time between
    [`TRIAL-START`][a231] and the [`VERDICT`][5976].
    
    Timing information is not available for `TRIAL-START` and [`ERROR*`][e6dd]
    events.

<a id='x-28TRY-3A-2APRINT-COMPACTLY-2A-20VARIABLE-29'></a>

- [variable] **\*PRINT-COMPACTLY\*** *NIL*

    [`EVENT`][6ded]s whose type matches `*PRINT-COMPACTLY*` are printed less
    verbosely. [`LEAF`][1ec2] events are printed only with their marker, and
    [`VERDICT`][5976]s of trials without printed child trials are printed with `=>
    <MARKER>` (see [`*CATEGORIES*`][3d4c]).
    
    ```
    (let ((*print-compactly* t)
          (*debug* nil)
          (*describe* nil))
      (with-test (outer)
        (loop repeat 10 do (is t))
        (with-test (inner)
          (is t)
          (is nil)
          (error "xxx"))
        (loop repeat 10 do (is t))))
    .. OUTER ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
    ..   INNER ⋅⊠⊟ => ⊟
    ..   ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
    .. ⊠ OUTER ⊟1 ⊠1 ⋅21
    ..
    ==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊟1 ⊠1 ⋅21>
    ```
    
    `*PRINT-COMPACTLY*` has no effect on events being `DESCRIBE`d.

<a id='x-28TRY-3A-2ADEFER-DESCRIBE-2A-20VARIABLE-29'></a>

- [variable] **\*DEFER-DESCRIBE\*** *NIL*

    When an [`EVENT`][6ded] is to be `DESCRIBE`d and its type matches
    `*DEFER-DESCRIBE*`, then instead of printing the often longish context
    information in the tree of events, it is deferred until after [`TRY`][7a62]
    has finished. The following example only prints [`LEAF`][1ec2] events (due to
    [`*PRINT*`][406c] and [`*PRINT-PARENT*`][85d7]) and in compact form (see
    [`*PRINT-COMPACTLY*`][7cd8]), deferring description of events matching
    [`*DESCRIBE*`][95df] until the end.
    
    ```
    (let ((*print* 'leaf)
          (*print-parent* nil)
          (*print-compactly* t)
          (*defer-describe* t)
          (*debug* nil))
      (with-test (outer)
        (loop repeat 10 do (is t))
        (with-test (inner)
          (is (= (1+ 5) 7)))))
    .. ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⊠
    ..
    .. ;; UNEXPECTED-RESULT-FAILURE (⊠) in OUTER INNER:
    .. (IS (= #1=(1+ 5) 7))
    .. where
    ..   #1# = 6
    ..
    ==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅10>
    ```


<a id='x-28TRY-3A-40TRY-2FCOUNT-20MGL-PAX-3ASECTION-29'></a>

### 7.4 Counting Events

[`TRIAL`][9fc3]s have a counter for each category in [`*CATEGORIES*`][3d4c]. When an
[`EVENT`][6ded] is recorded by [`TRY`][7a62] and its type matches [`*COUNT*`][7eb2], the counters
of all categories matching the event type are incremented in the
[`CURRENT-TRIAL`][e542]. When a trial finishes and a [`VERDICT`][5976] is recorded, the
trial's event counters are added to that of its parent's (if any).
The counts are printed with `VERDICT`s (see [Printing Events][53fc]).

If both `*COUNT*` and `*CATEGORIES*` are unchanged from the their
default values, then only [`LEAF`][1ec2] events are counted, and we get
separate counters for [`ABORT*`][cca5], [`UNEXPECTED-FAILURE`][10b2],
[`UNEXPECTED-SUCCESS`][ed00], [`SKIP`][5918], [`EXPECTED-FAILURE`][f526], and [`EXPECTED-SUCCESS`][7de6].

```
(let ((*debug* nil))
  (with-test (outer)
    (with-test (inner)
      (is t))
    (is t)
    (is nil)))
.. OUTER
..   INNER
..     ⋅ (IS T)
..   ⋅ INNER ⋅1
..   ⋅ (IS T)
..   ⊠ (IS NIL)
.. ⊠ OUTER ⊠1 ⋅2
..
==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅2>
```

As the above example shows, [`EXPECTED-VERDICT-SUCCESS`][1431] and
[`EXPECTED-RESULT-SUCCESS`][1c37] are both marked with `"⋅"`, but only
`EXPECTED-RESULT-SUCCESS` is counted due to `*COUNT*` being `LEAF`.

<a id='x-28TRY-3A-40TRY-2FCOLLECT-20MGL-PAX-3ASECTION-29'></a>

### 7.5 Collecting Events

When an [`EVENT`][6ded] is recorded and the type of the `EVENT` matches the
`COLLECT` type argument of [`TRY`][7a62], then a corresponding object is pushed
onto [`CHILDREN`][e616] of the [`CURRENT-TRIAL`][e542] for subsequent [Rerunning Trials][76af] or
[Reprocessing Trials][b0a8].

In particular, if the matching event is a [`LEAF`][1ec2], then the event
itself is collected. If the matching event is a [`TRIAL-EVENT`][fa53], then
its [`TRIAL`][6a27] is collected. Furthermore, trials
which collected anything are always collected by their parent.

By default, both implicit and explicit calls to `TRY` collect the
[`UNEXPECTED`][077b] (see [`*COLLECT*`][1dfa] and [`*TRY-COLLECT*`][9379]), and consequently all
the enclosing trials.

<a id='x-28TRY-3ACHILDREN-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29'></a>

- [reader] **CHILDREN** *TRIAL* *(:CHILDREN = NIL)*

    A list of immediate child [`VERDICT`][5976]s, [`RESULT`][3091]s, and
    [`ERROR*`][e6dd]s collected in reverse chronological order (see
    [Collecting Events][0a38]). The `VERDICT` of this [`TRIAL`][9fc3] is not among `CHILDREN`,
    but the `VERDICT`s of child trials' are.

<a id='x-28TRY-3A-40TRY-2FRERUN-20MGL-PAX-3ASECTION-29'></a>

### 7.6 Rerunning Trials

When a [`TRIAL`][9fc3] is `FUNCALL`ed or passed to [`TRY`][7a62], the same tests that
instantiated `TRIAL` are executed.

- If the trial was created by calling a [`DEFTEST`][e6a7] function, then the
  test currently associated with that symbol naming the function is
  called with the arguments of the original function call. If the
  symbol is no longer `FBOUNDP` (because it was `FMAKUNBOUND`) or it no
  longer names a `DEFTEST` (it was redefined with `DEFUN`), then an
  error is signalled.

- If the trial was created by entering a [`WITH-TEST`][af8d] form, then its
  body is executed again in the original lexical but the current
  dynamic environment. Implementationally speaking, `WITH-TEST`
  defines a local function of no arguments (likely a closure) that
  wraps its body, stores the closure in the trial object and calls
  it on a rerun in a `WITH-TEST` of the same `TRIAL-VAR` and same `NAME`.

- If the trial was created by `TRY` itself to ensure that all events
  are signalled in a trial (see [Explicit TRY][6c25]), then on a rerun
  the same `TESTABLE` is run again.

All three possibilities involve entering `DEFTEST` or `WITH-TEST`, or
invoking `TRY`: the same cases that we have when calling tests
functions (see [Calling Test Functions][0fb2]). Thus even if a trial is rerun
with `FUNCALL`, execution is guaranteed to happen under an `TRY`, so we
can talk about its `RERUN` parameter.

As the test functions are being rerun, some trials are automatically
skipped. When that happens the corresponding function call
immediately returns the `TRIAL` object. A new trial is skipped if

- among the collected but not yet rerun trials in the trial being
  rerun, there is no trial representing the same function call, or

- the first such trial does not match the `RERUN` type argument of `TRY`
  in that neither its [`TRIAL-START`][a231], [`VERDICT`][5976] events match the type
  `RERUN`, nor do any of its collected [`RESULT`][3091]s and trials.


<a id='x-28TRY-3A-40TRY-2FREPLAY-20MGL-PAX-3ASECTION-29'></a>

### 7.7 Reprocessing Trials

<a id='x-28TRY-3AREPLAY-EVENTS-20FUNCTION-29'></a>

- [function] **REPLAY-EVENTS** *TRIAL &KEY (COLLECT \*TRY-COLLECT\*) (PRINT \*TRY-PRINT\*) (DESCRIBE \*TRY-DESCRIBE\*) (STREAM \*TRY-STREAM\*) (PRINTER \*TRY-PRINTER\*)*

    `REPLAY-EVENTS` reprocesses the events collected (see [Collecting Events][0a38])
    in `TRIAL`. It takes the same arguments as [`TRY`][7a62] except `DEBUG`, `COUNT` and
    `RERUN`. This is because `REPLAY-EVENTS` does not run any tests. It
    simply signals the events collected in `TRIAL` again to allow further
    processing. The values of [`*CATEGORIES*`][3d4c] and [`*COUNT*`][7eb2] that were in
    effect for `TRIAL` are used, and their current values are ignored to
    be able to keep consistent counts (see [Counting Events][3c27]).
    
    Suppose we have run a large test using the default `:PRINT 'LEAF`
    `:COLLECT 'UNEXPECTED` arguments for `TRY`, and now we have too much
    output to look at. Instead of searching for the interesting bits in
    the output, we can replay the events and print only the [`UNEXPECTED`][077b]
    events:
    
    ```
    (replay-events ! :print 'unexpected)
    ```
    
    Or we could tell the printer to just print markers for `*CATEGORIES*`
    and `:DESCRIBE` at the end:
    
    ```
    (let ((*print-parent* nil)
          (*print-compactly* t)
          (*defer-describe* t)
          (*categories* (ascii-std-categories)))
      (replay-events !))
    .. ................F................!.....
    .. 
    .. ;; UNEXPECTED-FAILURE (F) in SOME-TEST INNER-TEST:
    .. (IS (= 5 6))
    .. debug info
    .. 
    .. ;; UNHANDLED-ERROR (!) in SOME-TEST:
    .. "my-msg" (MY-ERR)
    ```


<a id='x-28TRY-3A-40TRY-2FIMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29'></a>

## 8 Implementation Notes

Try is supported on ABCL, AllegroCL, CLISP, CCL, CMUCL, ECL and
SBCL.

- Pretty printing is non-existent on CLISP and broken on ABCL. The
  output may look garbled on them.

- Gray streams are broken on ABCL so the output may look even worse
  [https://abcl.org/trac/ticket/373](https://abcl.org/trac/ticket/373).

- ABCL, CMUCL, and ECL have a bug related to losing `EQL`ness of
  source literals
  [https://gitlab.com/embeddable-common-lisp/ecl/-/issues/665](https://gitlab.com/embeddable-common-lisp/ecl/-/issues/665).
  The result is somewhat cosmetic, it may cause multiple captures
  being made for the same thing.


<a id='x-28TRY-3A-40TRY-2FGLOSSARY-20MGL-PAX-3ASECTION-29'></a>

## 9 Glossary

<a id='x-28TRY-3A-3A-40FUNCTION-DESIGNATOR-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **function designator**

    This is a term from the Common Lisp ANSI standard. A function
    designator is a symbol (denoting the function named by that symbol
    in the global environment), or a function (denoting itself).

<a id='x-28TRY-3A-3A-40FUNCALLABLE-INSTANCE-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **funcallable instance**

    This is a term from the MOP. A funcallable instance is an instance
    of a class that's a subclass of `MOP:FUNCALLABLE-STANDARD-CLASS`. It
    is like a normal instance, but it can also be `FUNCALL`ed.

<a id='x-28TRY-3A-3A-40NON-LOCAL-EXIT-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **non-local exit**

    This is a term from the Common Lisp ANSI standard. If a form does
    not return normally, but control is transferred via `GO`, `RETURN`,
    `RETURN-FROM` or `THROW`, then it is said to have performed a non-local
    exit.

<a id='x-28TRY-3A-3A-40CANCELLED-NLX-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **cancelled non-local exit**

    This is a term from the Common Lisp ANSI standard. If during the
    unwinding of the stack initiated by a [non-local exit][c4d2] another nlx is
    initiated in, and exits from an `UNWIND-PROTECT` cleanup form, then
    this second nlx is said to have cancelled the first, and the first
    nlx will not continue.
    
    ```
    (catch 'foo
      (catch 'bar
        (unwind-protect
             (throw 'foo 'foo)
          (throw 'bar 'bar))))
    => BAR
    ```


  [0482]: #x-28TRY-3A-2ADEBUG-2A-20VARIABLE-29 "(TRY:*DEBUG* VARIABLE)"
  [068f]: #x-28TRY-3ASET-TRY-DEBUG-20FUNCTION-29 "(TRY:SET-TRY-DEBUG FUNCTION)"
  [077b]: #x-28TRY-3AUNEXPECTED-20CONDITION-29 "(TRY:UNEXPECTED CONDITION)"
  [0982]: #x-28TRY-3ASKIP-TRIAL-20FUNCTION-29 "(TRY:SKIP-TRIAL FUNCTION)"
  [0a38]: #x-28TRY-3A-40TRY-2FCOLLECT-20MGL-PAX-3ASECTION-29 "Collecting Events"
  [0d2f]: #x-28TRY-3ARETRY-CHECK-20FUNCTION-29 "(TRY:RETRY-CHECK FUNCTION)"
  [0e2a]: #x-28TRY-3A-2ARERUN-2A-20VARIABLE-29 "(TRY:*RERUN* VARIABLE)"
  [0fb2]: #x-28TRY-3A-40TRY-2FIMPLICIT-TRY-20MGL-PAX-3ASECTION-29 "Calling Test Functions"
  [10a6]: #x-28TRY-3A-2AN-RECENT-TRIALS-2A-20VARIABLE-29 "(TRY:*N-RECENT-TRIALS* VARIABLE)"
  [10b2]: #x-28TRY-3AUNEXPECTED-FAILURE-20TYPE-29 "(TRY:UNEXPECTED-FAILURE TYPE)"
  [1431]: #x-28TRY-3AEXPECTED-VERDICT-SUCCESS-20CONDITION-29 "(TRY:EXPECTED-VERDICT-SUCCESS CONDITION)"
  [14a6]: #x-28TRY-3A-40TRY-2FOUTCOME-RESTARTS-20MGL-PAX-3ASECTION-29 "Outcome Restarts"
  [16a7]: #x-28TRY-3AEXPECTED-RESULT-FAILURE-20CONDITION-29 "(TRY:EXPECTED-RESULT-FAILURE CONDITION)"
  [1955]: #x-28TRY-3A-40TRY-2FCHECKING-CONDITIONS-20MGL-PAX-3ASECTION-29 "Checking Conditions"
  [19eb]: #x-28TRY-3A-40TRY-2FTRIAL-EVENTS-20MGL-PAX-3ASECTION-29 "Trial Events"
  [1a75]: #x-28TRY-3ASIGNALS-NOT-20MGL-PAX-3AMACRO-29 "(TRY:SIGNALS-NOT MGL-PAX:MACRO)"
  [1c37]: #x-28TRY-3AEXPECTED-RESULT-SUCCESS-20CONDITION-29 "(TRY:EXPECTED-RESULT-SUCCESS CONDITION)"
  [1d8e]: #x-28TRY-3A-2ACONDITION-MATCHED-P-2A-20VARIABLE-29 "(TRY:*CONDITION-MATCHED-P* VARIABLE)"
  [1dfa]: #x-28TRY-3A-2ACOLLECT-2A-20VARIABLE-29 "(TRY:*COLLECT* VARIABLE)"
  [1ec2]: #x-28TRY-3ALEAF-20CONDITION-29 "(TRY:LEAF CONDITION)"
  [1f28]: #x-28TRY-3AMATCH-VALUES-20MGL-PAX-3AMACRO-29 "(TRY:MATCH-VALUES MGL-PAX:MACRO)"
  [20b7]: #x-28TRY-3AFAILS-20MGL-PAX-3AMACRO-29 "(TRY:FAILS MGL-PAX:MACRO)"
  [22e6]: #x-28TRY-3A-40TRY-2FFORMAT-SPECIFIER-FORMS-20MGL-PAX-3ASECTION-29 "Format Specifier Forms"
  [24b5]: #x-28TRY-3AVERDICT-SKIP-20CONDITION-29 "(TRY:VERDICT-SKIP CONDITION)"
  [300f]: #x-28TRY-3A-40TRY-2FIMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29 "Implementation Notes"
  [303f]: #x-28TRY-3A-40TRY-2FIMPLICIT-TRY-IMPLEMENTATION-20MGL-PAX-3ASECTION-29 "Implementation of Implicit TRY"
  [3091]: #x-28TRY-3ARESULT-20CONDITION-29 "(TRY:RESULT CONDITION)"
  [318d]: #x-28TRY-3ARECORD-EVENT-20FUNCTION-29 "(TRY:RECORD-EVENT FUNCTION)"
  [338d]: #x-28TRY-3APASS-20TYPE-29 "(TRY:PASS TYPE)"
  [3c27]: #x-28TRY-3A-40TRY-2FCOUNT-20MGL-PAX-3ASECTION-29 "Counting Events"
  [3d4c]: #x-28TRY-3A-2ACATEGORIES-2A-20-28VARIABLE-20-22--20see-20above-20--22-29-29 "(TRY:*CATEGORIES* (VARIABLE \"- see above -\"))"
  [3e2d]: #x-28TRY-3A-40TRY-2FCHECKS-20MGL-PAX-3ASECTION-29 "Checks"
  [3ea6]: #x-28TRY-3A-40TRY-2FMISC-CHECKS-20MGL-PAX-3ASECTION-29 "Miscellaneous Checks"
  [406c]: #x-28TRY-3A-2APRINT-2A-20VARIABLE-29 "(TRY:*PRINT* VARIABLE)"
  [440d]: #x-28TRY-3ASUCCESS-20CONDITION-29 "(TRY:SUCCESS CONDITION)"
  [44ee]: #x-28TRY-3ASKIP-CHECK-20FUNCTION-29 "(TRY:SKIP-CHECK FUNCTION)"
  [4c47]: #x-28TRY-3ARESULT-ABORT-2A-20CONDITION-29 "(TRY:RESULT-ABORT* CONDITION)"
  [4cbe]: #x-28TRY-3AVERDICT-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29 "(TRY:VERDICT (MGL-PAX:READER TRY:TRIAL))"
  [4f4d]: #x-28TRY-3ACAPTURE-20MGL-PAX-3AMACRO-29 "(TRY:CAPTURE MGL-PAX:MACRO)"
  [4fbb]: #x-28TRY-3A-40TRY-2FCHECK-LIBRARY-20MGL-PAX-3ASECTION-29 "Check Library"
  [51bb]: #x-28TRY-3A-40TRY-2FTESTS-20MGL-PAX-3ASECTION-29 "Tests"
  [532a]: #x-28TRY-3A-40TRY-2FAUTOMATIC-CAPTURES-20MGL-PAX-3ASECTION-29 "Automatic Captures"
  [53fc]: #x-28TRY-3A-40TRY-2FPRINT-20MGL-PAX-3ASECTION-29 "Printing Events"
  [57b2]: #x-28TRY-3A-40TRY-2FEVENT-GLUE-20MGL-PAX-3ASECTION-29 "Event Glue"
  [5918]: #x-28TRY-3ASKIP-20CONDITION-29 "(TRY:SKIP CONDITION)"
  [5976]: #x-28TRY-3AVERDICT-20CONDITION-29 "(TRY:VERDICT CONDITION)"
  [6249]: #x-28TRY-3AIN-TIME-20MGL-PAX-3AMACRO-29 "(TRY:IN-TIME MGL-PAX:MACRO)"
  [65b2]: #x-28-23A-28-283-29-20BASE-CHAR-20-2E-20-22try-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29 "(#A((3) BASE-CHAR . \"try\") ASDF/SYSTEM:SYSTEM)"
  [67de]: #x-28TRY-3ACAPTURE-VALUES-20MGL-PAX-3AMACRO-29 "(TRY:CAPTURE-VALUES MGL-PAX:MACRO)"
  [6866]: #x-28TRY-3A-2APRINTER-2A-20VARIABLE-29 "(TRY:*PRINTER* VARIABLE)"
  [68a8]: #x-28TRY-3A-40TRY-2FEVENTS-20MGL-PAX-3ASECTION-29 "Events"
  [68db]: #x-28TRY-3ADISMISSAL-20CONDITION-29 "(TRY:DISMISSAL CONDITION)"
  [6909]: #x-28TRY-3AN-RETRIES-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29 "(TRY:N-RETRIES (MGL-PAX:READER TRY:TRIAL))"
  [6a27]: #x-28TRY-3ATRIAL-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-EVENT-29-29 "(TRY:TRIAL (MGL-PAX:READER TRY:TRIAL-EVENT))"
  [6b07]: #x-28TRY-3A-40TRY-2FCHECK-UTILITIES-20MGL-PAX-3ASECTION-29 "Check Utilities"
  [6c25]: #x-28TRY-3A-40TRY-2FEXPLICIT-TRY-20MGL-PAX-3ASECTION-29 "Explicit TRY"
  [6ded]: #x-28TRY-3AEVENT-20CONDITION-29 "(TRY:EVENT CONDITION)"
  [6ebb]: #x-28TRY-3ASIGNALS-20MGL-PAX-3AMACRO-29 "(TRY:SIGNALS MGL-PAX:MACRO)"
  [70d8]: #x-28TRY-3A-2ATRY-DEBUG-2A-20VARIABLE-29 "(TRY:*TRY-DEBUG* VARIABLE)"
  [7397]: #x-28TRY-3AFLOAT--7E-3D-20FUNCTION-29 "(TRY:FLOAT-~= FUNCTION)"
  [73e6]: #x-28TRY-3AREPLAY-EVENTS-20FUNCTION-29 "(TRY:REPLAY-EVENTS FUNCTION)"
  [74b0]: #x-28TRY-3A-2AIS-CAPTURES-2A-20VARIABLE-29 "(TRY:*IS-CAPTURES* VARIABLE)"
  [7694]: #x-28TRY-3AEXPECTED-20CONDITION-29 "(TRY:EXPECTED CONDITION)"
  [76af]: #x-28TRY-3A-40TRY-2FRERUN-20MGL-PAX-3ASECTION-29 "Rerunning Trials"
  [7831]: #x-28TRY-3AFAIL-20TYPE-29 "(TRY:FAIL TYPE)"
  [7a62]: #x-28TRY-3ATRY-20FUNCTION-29 "(TRY:TRY FUNCTION)"
  [7cd8]: #x-28TRY-3A-2APRINT-COMPACTLY-2A-20VARIABLE-29 "(TRY:*PRINT-COMPACTLY* VARIABLE)"
  [7de6]: #x-28TRY-3AEXPECTED-SUCCESS-20TYPE-29 "(TRY:EXPECTED-SUCCESS TYPE)"
  [7eb2]: #x-28TRY-3A-2ACOUNT-2A-20VARIABLE-29 "(TRY:*COUNT* VARIABLE)"
  [7f53]: #x-28TRY-3A-40TRY-2FTUTORIAL-20MGL-PAX-3ASECTION-29 "Tutorial"
  [808e]: #x-28TRY-3A-40TRY-2FIS-20MGL-PAX-3ASECTION-29 "The IS Macro"
  [8292]: #x-28TRY-3A-40TRY-2FGLOSSARY-20MGL-PAX-3ASECTION-29 "Glossary"
  [8389]: #x-28TRY-3AWITH-TESTS-RUN-20MGL-PAX-3AMACRO-29 "(TRY:WITH-TESTS-RUN MGL-PAX:MACRO)"
  [85d7]: #x-28TRY-3A-2APRINT-PARENT-2A-20VARIABLE-29 "(TRY:*PRINT-PARENT* VARIABLE)"
  [87a0]: #x-28TRY-3AUNEXPECTED-RESULT-SUCCESS-20CONDITION-29 "(TRY:UNEXPECTED-RESULT-SUCCESS CONDITION)"
  [87f6]: #x-28TRY-3AON-VALUES-20MGL-PAX-3AMACRO-29 "(TRY:ON-VALUES MGL-PAX:MACRO)"
  [888d]: #x-28TRY-3A-2ASTREAM-2A-20-28VARIABLE-20-28MAKE-SYNONYM-STREAM-20-28QUOTE-20-2ADEBUG-IO-2A-29-29-29-29 "(TRY:*STREAM* (VARIABLE (MAKE-SYNONYM-STREAM (QUOTE *DEBUG-IO*))))"
  [8a03]: #x-28TRY-3A-40TRY-2FCONCRETE-EVENTS-20MGL-PAX-3ASECTION-29 "Concrete Events"
  [8a79]: #x-28TRY-3A-40TRY-2FEXPLICIT-CAPTURES-20MGL-PAX-3ASECTION-29 "Explicit Captures"
  [8ce0]: #x-28TRY-3AUNEXPECTED-RESULT-FAILURE-20CONDITION-29 "(TRY:UNEXPECTED-RESULT-FAILURE CONDITION)"
  [8def]: #x-28TRY-3A-2ARUN-DEFTEST-WHEN-2A-20VARIABLE-29 "(TRY:*RUN-DEFTEST-WHEN* VARIABLE)"
  [9379]: #x-28TRY-3A-2ATRY-COLLECT-2A-20VARIABLE-29 "(TRY:*TRY-COLLECT* VARIABLE)"
  [93e2]: #x-28TRY-3ARETRY-TRIAL-20FUNCTION-29 "(TRY:RETRY-TRIAL FUNCTION)"
  [9539]: #x-28TRY-3AINVOKES-DEBUGGER-20MGL-PAX-3AMACRO-29 "(TRY:INVOKES-DEBUGGER MGL-PAX:MACRO)"
  [953d]: #x-28TRY-3A-40TRY-2FPRINTING-EVENTS-20MGL-PAX-3ASECTION-29 "Printing Events"
  [95c4]: #x-28TRY-3A-40TRY-2FOUTCOMES-20MGL-PAX-3ASECTION-29 "Outcomes"
  [95df]: #x-28TRY-3A-2ADESCRIBE-2A-20VARIABLE-29 "(TRY:*DESCRIBE* VARIABLE)"
  [989e]: #x-28TRY-3ARESULT-SKIP-20CONDITION-29 "(TRY:RESULT-SKIP CONDITION)"
  [9e9e]: #x-28TRY-3ALIST-PACKAGE-TESTS-20FUNCTION-29 "(TRY:LIST-PACKAGE-TESTS FUNCTION)"
  [9f9b]: #x-28TRY-3A-2ABEST-MATCHING-CONDITION-2A-20VARIABLE-29 "(TRY:*BEST-MATCHING-CONDITION* VARIABLE)"
  [9fc3]: #x-28TRY-3ATRIAL-20TYPE-29 "(TRY:TRIAL TYPE)"
  [a03c]: #x-28TRY-3A-40TRY-2FCAPTURES-20MGL-PAX-3ASECTION-29 "Captures"
  [a19c]: #x-28TRY-3AWARN-ON-TESTS-NOT-RUN-20MGL-PAX-3AMACRO-29 "(TRY:WARN-ON-TESTS-NOT-RUN MGL-PAX:MACRO)"
  [a231]: #x-28TRY-3ATRIAL-START-20CONDITION-29 "(TRY:TRIAL-START CONDITION)"
  [a306]: #x-28TRY-3AOUTCOME-20CONDITION-29 "(TRY:OUTCOME CONDITION)"
  [a53e]: #x-28TRY-3A-40TRY-2FLINKS-20MGL-PAX-3ASECTION-29 "Links"
  [a57a]: #x-28TRY-3A-40TRY-2FMIDDLE-LAYER-OF-EVENTS-20MGL-PAX-3ASECTION-29 "Middle Layer of Events"
  [ab7a]: #x-28TRY-3AWITH-EXPECTED-OUTCOME-20MGL-PAX-3AMACRO-29 "(TRY:WITH-EXPECTED-OUTCOME MGL-PAX:MACRO)"
  [abcf]: #x-28TRY-3A-40TRY-2FTRIALS-20MGL-PAX-3ASECTION-29 "Trials"
  [af8d]: #x-28TRY-3AWITH-TEST-20MGL-PAX-3AMACRO-29 "(TRY:WITH-TEST MGL-PAX:MACRO)"
  [afcd]: #x-28TRY-3A-40TRY-2FWRITING-AUTOMATIC-CAPTURE-RULES-20MGL-PAX-3ASECTION-29 "Writing Automatic Capture Rules"
  [b0a8]: #x-28TRY-3A-40TRY-2FREPLAY-20MGL-PAX-3ASECTION-29 "Reprocessing Trials"
  [b12f]: #x-28TRY-3AINVOKES-DEBUGGER-NOT-20MGL-PAX-3AMACRO-29 "(TRY:INVOKES-DEBUGGER-NOT MGL-PAX:MACRO)"
  [b594]: #x-28TRY-3A-40TRY-2FCOMPARING-FLOATS-20MGL-PAX-3ASECTION-29 "Comparing Floats"
  [b858]: #x-28TRY-3A-25-25-20MACROLET-29 "(TRY:%% MACROLET)"
  [bbc4]: #x-28TRY-3A-40TRY-2FERRORS-20MGL-PAX-3ASECTION-29 "Errors"
  [bd26]: #x-28TRY-3AUNEXPECTED-VERDICT-SUCCESS-20CONDITION-29 "(TRY:UNEXPECTED-VERDICT-SUCCESS CONDITION)"
  [c04d]: #x-28TRY-3AUNEXPECTED-VERDICT-FAILURE-20CONDITION-29 "(TRY:UNEXPECTED-VERDICT-FAILURE CONDITION)"
  [c4ab]: #x-28TRY-3A-2ATRY-PRINTER-2A-20VARIABLE-29 "(TRY:*TRY-PRINTER* VARIABLE)"
  [c4d2]: #x-28TRY-3A-3A-40NON-LOCAL-EXIT-20MGL-PAX-3AGLOSSARY-TERM-29 "(TRY::@NON-LOCAL-EXIT MGL-PAX:GLOSSARY-TERM)"
  [c705]: #x-28TRY-3AABORT-TRIAL-20FUNCTION-29 "(TRY:ABORT-TRIAL FUNCTION)"
  [c773]: #x-28TRY-3ARUNNINGP-20FUNCTION-29 "(TRY:RUNNINGP FUNCTION)"
  [ca88]: #x-28TRY-3AVERDICT-ABORT-2A-20CONDITION-29 "(TRY:VERDICT-ABORT* CONDITION)"
  [cca5]: #x-28TRY-3AABORT-2A-20CONDITION-29 "(TRY:ABORT* CONDITION)"
  [cd54]: #x-28TRY-3AWITH-SKIP-20MGL-PAX-3AMACRO-29 "(TRY:WITH-SKIP MGL-PAX:MACRO)"
  [d0fc]: #x-28TRY-3A-21-20-28VARIABLE-20NIL-29-29 "(TRY:! (VARIABLE NIL))"
  [d270]: #x-28TRY-3A-40TRY-2FTESTABLES-20MGL-PAX-3ASECTION-29 "Testables"
  [d43d]: #x-28TRY-3ANLX-20CONDITION-29 "(TRY:NLX CONDITION)"
  [d65b]: #x-28TRY-3A-3A-40CANCELLED-NLX-20MGL-PAX-3AGLOSSARY-TERM-29 "(TRY::@CANCELLED-NLX MGL-PAX:GLOSSARY-TERM)"
  [d94b]: #x-28TRY-3A-25-20MACROLET-29 "(TRY:% MACROLET)"
  [dbd2]: #x-28TRY-3A-40TRY-2FCHECK-RESTARTS-20MGL-PAX-3ASECTION-29 "Check Restarts"
  [dbf0]: #x-28TRY-3A-40TRY-2FTRIAL-RESTARTS-20MGL-PAX-3ASECTION-29 "Trial Restarts"
  [de86]: #x-28TRY-3A-3A-40FUNCALLABLE-INSTANCE-20MGL-PAX-3AGLOSSARY-TERM-29 "(TRY::@FUNCALLABLE-INSTANCE MGL-PAX:GLOSSARY-TERM)"
  [e019]: #x-28TRY-3ABACKTRACE-OF-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29 "(TRY:BACKTRACE-OF (MGL-PAX:READER TRY:UNHANDLED-ERROR))"
  [e542]: #x-28TRY-3ACURRENT-TRIAL-20FUNCTION-29 "(TRY:CURRENT-TRIAL FUNCTION)"
  [e55e]: #x-28TRY-3ATEST-BOUND-P-20FUNCTION-29 "(TRY:TEST-BOUND-P FUNCTION)"
  [e616]: #x-28TRY-3ACHILDREN-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29 "(TRY:CHILDREN (MGL-PAX:READER TRY:TRIAL))"
  [e65b]: #x-28TRY-3AUNHANDLED-ERROR-20CONDITION-29 "(TRY:UNHANDLED-ERROR CONDITION)"
  [e6a7]: #x-28TRY-3ADEFTEST-20MGL-PAX-3AMACRO-29 "(TRY:DEFTEST MGL-PAX:MACRO)"
  [e6dd]: #x-28TRY-3AERROR-2A-20CONDITION-29 "(TRY:ERROR* CONDITION)"
  [e852]: #x-28TRY-3A-40TRY-2FEVENT-RESTARTS-20MGL-PAX-3ASECTION-29 "Event Restarts"
  [ea26]: #x-28TRY-3A-40TRY-2FTRIAL-VERDICTS-20MGL-PAX-3ASECTION-29 "Trial Verdicts"
  [ea4a]: #x-28TRY-3AFAILURE-20CONDITION-29 "(TRY:FAILURE CONDITION)"
  [eb00]: #x-28TRY-3AABORT-CHECK-20FUNCTION-29 "(TRY:ABORT-CHECK FUNCTION)"
  [ed00]: #x-28TRY-3AUNEXPECTED-SUCCESS-20TYPE-29 "(TRY:UNEXPECTED-SUCCESS TYPE)"
  [efc2]: #x-28TRY-3A-3A-40FUNCTION-DESIGNATOR-20MGL-PAX-3AGLOSSARY-TERM-29 "(TRY::@FUNCTION-DESIGNATOR MGL-PAX:GLOSSARY-TERM)"
  [f1c6]: #x-28TRY-3A-40TRY-2FCATEGORIES-20MGL-PAX-3ASECTION-29 "Categories"
  [f526]: #x-28TRY-3AEXPECTED-FAILURE-20TYPE-29 "(TRY:EXPECTED-FAILURE TYPE)"
  [fa53]: #x-28TRY-3ATRIAL-EVENT-20CONDITION-29 "(TRY:TRIAL-EVENT CONDITION)"
  [fec9]: #x-28TRY-3AEXPECTED-VERDICT-FAILURE-20CONDITION-29 "(TRY:EXPECTED-VERDICT-FAILURE CONDITION)"
  [ff02]: #x-28TRY-3AIS-20MGL-PAX-3AMACRO-29 "(TRY:IS MGL-PAX:MACRO)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
